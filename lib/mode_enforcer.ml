type mutation_class =
  | Read_only
  | Workspace_mutating
  | External_effect

type violation_kind =
  | Mutating_in_diagnose
  | External_in_draft
  | Scope_violation

let violation_kind_to_string = function
  | Mutating_in_diagnose -> "mutating_in_diagnose"
  | External_in_draft -> "external_in_draft"
  | Scope_violation -> "scope_violation"

let violation_kind_of_string = function
  | "mutating_in_diagnose" -> Ok Mutating_in_diagnose
  | "external_in_draft" -> Ok External_in_draft
  | "scope_violation" -> Ok Scope_violation
  | s -> Error (Printf.sprintf "unknown violation_kind: %s" s)

let violation_kind_to_yojson v = `String (violation_kind_to_string v)

let violation_kind_of_yojson = function
  | `String s -> violation_kind_of_string s
  | j -> Error (Printf.sprintf "expected string, got %s" (Yojson.Safe.to_string j))

type violation = {
  ts: float;
  tool_name: string;
  input_summary: string;
  effective_mode: Execution_mode.t;
  violation_kind: violation_kind;
}

let violation_to_yojson v =
  `Assoc [
    "ts", `Float v.ts;
    "tool_name", `String v.tool_name;
    "input_summary", `String v.input_summary;
    "effective_mode", Execution_mode.to_yojson v.effective_mode;
    "violation_kind", violation_kind_to_yojson v.violation_kind;
  ]

let violation_of_yojson = function
  | `Assoc fields ->
    (match
       List.assoc_opt "ts" fields,
       List.assoc_opt "tool_name" fields,
       List.assoc_opt "input_summary" fields,
       List.assoc_opt "effective_mode" fields,
       List.assoc_opt "violation_kind" fields
     with
     | Some (`Float ts), Some (`String tool_name),
       Some (`String input_summary), Some mode_json, Some kind_json ->
       (match Execution_mode.of_yojson mode_json, violation_kind_of_yojson kind_json with
        | Ok effective_mode, Ok violation_kind ->
          Ok { ts; tool_name; input_summary; effective_mode; violation_kind }
        | Error e, _ | _, Error e -> Error e)
     | _ -> Error "missing or invalid fields in violation")
  | _ -> Error "violation: expected JSON object"

type token_snapshot = {
  input_tokens: int;
  output_tokens: int;
  cost_usd: float option;
  turn: int;
}

type state = {
  effective_mode: Execution_mode.t;
  allowed_mutations: string list;
  review_requirement: string option;
  mutable violations: violation list;
  mutable token_snapshots: token_snapshot list;
  mutable review_warning: string option;
}

let create ~contract ~effective_mode =
  let rc = contract.Risk_contract.runtime_constraints in
  {
    effective_mode;
    allowed_mutations = rc.allowed_mutations;
    review_requirement = rc.review_requirement;
    violations = [];
    token_snapshots = [];
    review_warning = None;
  }

let violations st = List.rev st.violations
let token_snapshots st = List.rev st.token_snapshots
let review_warning st = st.review_warning

(* ── Tool classification ─────────────────────────────────────────── *)

let classify_tool name =
  match String.lowercase_ascii name with
  | "read" | "glob" | "grep" | "search" | "list_dir" | "find_file"
  | "read_file" | "find_symbol" | "get_symbols_overview"
  | "find_referencing_symbols" | "search_for_pattern"
  | "read_console_messages" | "read_network_requests"
  | "get_page_text" | "read_page" | "tabs_context_mcp" ->
    Read_only
  | "write" | "edit" | "create_text_file" | "replace_content"
  | "rename_symbol" | "insert_after_symbol" | "insert_before_symbol"
  | "replace_symbol_body" | "notebook_edit" ->
    Workspace_mutating
  | n when String.length n > 7
           && String.sub n 0 7 = "keeper_" ->
    Workspace_mutating
  | n when String.length n > 5
           && String.sub n 0 5 = "masc_" ->
    Workspace_mutating
  | n when String.length n > 5
           && String.sub n 0 5 = "mcp__" ->
    External_effect
  | _ -> External_effect

let has_any_pattern patterns haystack =
  List.exists (fun pat -> Util.string_contains ~needle:pat haystack) patterns

let mutating_patterns = [
  "rm "; "rm\t"; "rmdir "; "mv "; "cp "; "mkdir ";
  "touch "; "chmod "; "chown "; "dd "; "mkfs";
  "apt "; "brew "; "pip "; "npm "; "yarn "; "cargo ";
  "git push"; "git commit"; "git add"; "git reset";
  "git rebase"; "git merge"; "git checkout";
  "docker "; "kubectl "; "systemctl ";
  "curl "; "wget ";
]

let external_patterns = [
  "curl "; "wget "; "ssh "; "scp "; "rsync ";
  "git push"; "git fetch"; "git pull"; "git clone";
  "docker "; "kubectl "; "helm ";
  "npm publish"; "pip install"; "cargo publish";
  "systemctl "; "launchctl ";
]

let extract_bash_command (input : Yojson.Safe.t) =
  match input with
  | `Assoc fields ->
    (match List.assoc_opt "command" fields with
     | Some (`String cmd) -> Some (String.lowercase_ascii cmd)
     | _ -> None)
  | _ -> None

let is_bash_read_only input =
  match extract_bash_command input with
  | None -> false
  | Some cmd ->
    let has_redirect =
      String.contains cmd '>'
      || Util.string_contains ~needle:"tee " cmd
    in
    not (has_any_pattern mutating_patterns cmd) && not has_redirect

let is_bash_workspace_only input =
  match extract_bash_command input with
  | None -> false
  | Some cmd -> not (has_any_pattern external_patterns cmd)

let classify_bash_tool input =
  if is_bash_read_only input then Read_only
  else if is_bash_workspace_only input then Workspace_mutating
  else External_effect

let effective_class tool_name input =
  match classify_tool tool_name with
  | External_effect
    when String.lowercase_ascii tool_name = "bash"
      || String.lowercase_ascii tool_name = "execute_shell_command" ->
    classify_bash_tool input
  | c -> c

let all_read_only tools =
  List.for_all (fun name -> classify_tool name = Read_only) tools

let all_workspace_only tools =
  List.for_all (fun name ->
    match classify_tool name with
    | Read_only | Workspace_mutating -> true
    | External_effect -> false
  ) tools

(* ── Enforcement check ───────────────────────────────────────────── *)

let truncate_input input =
  Util.clip (Yojson.Safe.to_string input) 200

let check_violation st tool_name input =
  let cls = effective_class tool_name input in
  let kind = match st.effective_mode, cls with
    | Execution_mode.Diagnose, (Workspace_mutating | External_effect) ->
      Some Mutating_in_diagnose
    | Execution_mode.Draft, External_effect ->
      Some External_in_draft
    | _ ->
      if List.mem "workspace_only" st.allowed_mutations
         && cls = External_effect then
        Some Scope_violation
      else
        None
  in
  match kind with
  | None -> None
  | Some violation_kind ->
    let v = {
      ts = Unix.gettimeofday ();
      tool_name;
      input_summary = truncate_input input;
      effective_mode = st.effective_mode;
      violation_kind;
    } in
    st.violations <- v :: st.violations;
    Some v

(* ── Hooks ───────────────────────────────────────────────────────── *)

let hooks st =
  let open Hooks in
  {
    empty with

    before_turn = Some (fun event ->
      (match event with
       | BeforeTurn { turn = 1; _ } ->
         (match st.review_requirement, st.effective_mode with
          | Some req, Execution_mode.Execute ->
            st.review_warning <- Some (Printf.sprintf
              "review_requirement '%s' active but running in Execute mode" req)
          | _ -> ())
       | _ -> ());
      Continue);

    pre_tool_use = Some (fun event ->
      match event with
      | PreToolUse { tool_name; input; _ } ->
        (match check_violation st tool_name input with
         | Some _ -> Skip
         | None -> Continue)
      | _ -> Continue);

    after_turn = Some (fun event ->
      (match event with
       | AfterTurn { turn; response; _ } ->
         (match response.Types.usage with
          | Some u ->
            let snap = {
              input_tokens = u.input_tokens;
              output_tokens = u.output_tokens;
              cost_usd = u.cost_usd;
              turn;
            } in
            st.token_snapshots <- snap :: st.token_snapshots
          | None -> ())
       | _ -> ());
      Continue);
  }
