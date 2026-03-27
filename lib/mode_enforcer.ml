type mutation_class =
  | Read_only
  | Workspace_mutating
  | External_effect

type violation = {
  ts: float;
  tool_name: string;
  input_summary: string;
  effective_mode: Execution_mode.t;
  violation_kind: string;
}

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
  | n when String.length n > 5
           && String.sub n 0 5 = "mcp__" ->
    External_effect
  | _ -> External_effect

let is_bash_read_only (input : Yojson.Safe.t) =
  match input with
  | `Assoc fields ->
    (match List.assoc_opt "command" fields with
     | Some (`String cmd) ->
       let cmd_lower = String.lowercase_ascii cmd in
       let mutating_patterns = [
         "rm "; "rm\t"; "rmdir "; "mv "; "cp "; "mkdir ";
         "touch "; "chmod "; "chown "; "dd "; "mkfs";
         "apt "; "brew "; "pip "; "npm "; "yarn "; "cargo ";
         "git push"; "git commit"; "git add"; "git reset";
         "git rebase"; "git merge"; "git checkout";
         "docker "; "kubectl "; "systemctl ";
         "curl "; "wget ";
       ] in
       let has_redirect =
         String.contains cmd '>'
         || (try ignore (Str.search_forward (Str.regexp_string "tee ") cmd 0); true
             with Not_found -> false)
       in
       let has_mutating = List.exists (fun pat ->
         try ignore (Str.search_forward (Str.regexp_string pat) cmd_lower 0); true
         with Not_found -> false
       ) mutating_patterns in
       not has_mutating && not has_redirect
     | _ -> false)
  | _ -> false

let is_bash_workspace_only (input : Yojson.Safe.t) =
  match input with
  | `Assoc fields ->
    (match List.assoc_opt "command" fields with
     | Some (`String cmd) ->
       let cmd_lower = String.lowercase_ascii cmd in
       let external_patterns = [
         "curl "; "wget "; "ssh "; "scp "; "rsync ";
         "git push"; "git fetch"; "git pull"; "git clone";
         "docker "; "kubectl "; "helm ";
         "npm publish"; "pip install"; "cargo publish";
         "systemctl "; "launchctl ";
       ] in
       not (List.exists (fun pat ->
         try ignore (Str.search_forward (Str.regexp_string pat) cmd_lower 0); true
         with Not_found -> false
       ) external_patterns)
     | _ -> false)
  | _ -> false

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
  let s = Yojson.Safe.to_string input in
  if String.length s > 200 then String.sub s 0 200 ^ "..."
  else s

let check_violation st tool_name input =
  let cls = effective_class tool_name input in
  let kind = match st.effective_mode, cls with
    | Execution_mode.Diagnose, (Workspace_mutating | External_effect) ->
      Some "mutating_in_diagnose"
    | Execution_mode.Draft, External_effect ->
      Some "external_in_draft"
    | _ ->
      if List.mem "workspace_only" st.allowed_mutations
         && cls = External_effect then
        Some "scope_violation"
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
