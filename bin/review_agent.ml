(** review_agent — PR code review via OAS agent.

    Cmdliner-based CLI wrapping the review agent example.

    Usage:
      oas-review --repo owner/name --pr 123
      oas-review --repo owner/name --pr 123 --post
      oas-review --repo owner/name --pr 123 --provider anthropic *)

open Agent_sdk
open Types

(* ── Shell helper ──────────────────────────────────────────── *)

let run_gh_command args =
  let cmd = String.concat " " ("gh" :: List.map Filename.quote args) in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 4096 in
  (try while true do
     Buffer.add_string buf (input_line ic);
     Buffer.add_char buf '\n'
   done with End_of_file -> ());
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Ok (Buffer.contents buf)
  | Unix.WEXITED n -> Error (Printf.sprintf "gh exited with code %d: %s" n (Buffer.contents buf))
  | _ -> Error "gh process killed or stopped"

(* ── Tools ─────────────────────────────────────────────────── *)

let get_pr_info_tool =
  Tool.create ~name:"get_pr_info"
    ~description:"Get PR metadata: title, body, files changed, additions/deletions"
    ~parameters:[
      { name = "repo"; description = "owner/repo"; param_type = String; required = true };
      { name = "pr_number"; description = "PR number"; param_type = String; required = true };
    ]
    (fun args ->
      let open Yojson.Safe.Util in
      let repo = args |> member "repo" |> to_string in
      let pr_num = args |> member "pr_number" |> to_string in
      match run_gh_command ["pr"; "view"; pr_num; "--repo"; repo;
        "--json"; "title,body,additions,deletions,changedFiles,labels"] with
      | Ok output -> Ok { content = output }
      | Error msg -> Error { message = msg; recoverable = true })

let get_pr_diff_tool =
  Tool.create ~name:"get_pr_diff"
    ~description:"Get the unified diff of a PR. For large PRs, use file_filter to limit."
    ~parameters:[
      { name = "repo"; description = "owner/repo"; param_type = String; required = true };
      { name = "pr_number"; description = "PR number"; param_type = String; required = true };
      { name = "file_filter"; description = "Optional: only show diff for this file path";
        param_type = String; required = false };
    ]
    (fun args ->
      let open Yojson.Safe.Util in
      let repo = args |> member "repo" |> to_string in
      let pr_num = args |> member "pr_number" |> to_string in
      (match run_gh_command ["pr"; "diff"; pr_num; "--repo"; repo] with
      | Error msg -> Error { message = msg; recoverable = true }
      | Ok output ->
      let filtered = match args |> member "file_filter" |> to_string_option with
        | None -> output
        | Some path ->
          let lines = String.split_on_char '\n' output in
          let in_file = ref false in
          let buf = Buffer.create 2048 in
          List.iter (fun line ->
            if String.length line > 6 && String.sub line 0 6 = "diff -" then
              in_file := String.length line > 0 &&
                (try ignore (Str.search_forward (Str.regexp_string path) line 0); true
                 with Not_found -> false);
            if !in_file then begin
              Buffer.add_string buf line;
              Buffer.add_char buf '\n'
            end
          ) lines;
          Buffer.contents buf
      in
      let max_len = 12000 in
      let result = if String.length filtered > max_len then
        String.sub filtered 0 max_len ^ "\n... [truncated]"
      else filtered in
      Ok { content = result }))

let post_review_tool =
  Tool.create ~name:"post_review"
    ~description:"Post a review comment on the PR"
    ~parameters:[
      { name = "repo"; description = "owner/repo"; param_type = String; required = true };
      { name = "pr_number"; description = "PR number"; param_type = String; required = true };
      { name = "body"; description = "Review comment body (markdown)";
        param_type = String; required = true };
    ]
    (fun args ->
      let open Yojson.Safe.Util in
      let repo = args |> member "repo" |> to_string in
      let pr_num = args |> member "pr_number" |> to_string in
      let body = args |> member "body" |> to_string in
      let tmp = Filename.temp_file "review" ".md" in
      let oc = open_out tmp in
      output_string oc body;
      close_out oc;
      let result = run_gh_command ["pr"; "comment"; pr_num; "--repo"; repo; "--body-file"; tmp] in
      Sys.remove tmp;
      match result with
      | Ok output -> Ok { content = Printf.sprintf "Review posted. %s" (String.trim output) }
      | Error msg -> Error { message = msg; recoverable = true })

(* ── System prompt ─────────────────────────────────────────── *)

let system_prompt = {|You are a code review agent. Your workflow:

1. Use get_pr_info to understand the PR context (title, description, size)
2. Use get_pr_diff to read the actual changes
3. Analyze the diff for:
   - Bugs or logic errors (HIGH priority)
   - Missing error handling
   - Type safety issues
   - Performance concerns
   - Style/convention issues (LOW priority)
4. Output your review as structured markdown

Focus on issues that matter. Skip trivial style comments.
If the PR is clean, say so briefly.

Output format:
## Review: [PR title]
**Verdict**: LGTM | Minor Issues | Needs Changes
### Findings
- [severity] file:line — description
|}

(* ── Provider resolution ───────────────────────────────────── *)

let rec resolve_provider name =
  match name with
  | "anthropic" ->
    Provider.anthropic_sonnet ()
  | "local" | "" ->
    let base_url = match Sys.getenv_opt "LLM_BASE_URL" with
      | Some url -> url
      | None -> "http://127.0.0.1:8085"
    in
    Provider.({
      provider = OpenAICompat {
        base_url;
        auth_header = None;
        path = "/v1/chat/completions";
        static_token = None;
      };
      model_id = "qwen3.5";
      api_key_env = "";
    })
  | "openrouter" ->
    Provider.openrouter ()
  | other ->
    Printf.eprintf "Unknown provider: %s (using local)\n" other;
    resolve_provider "local"

(* ── Cmdliner ──────────────────────────────────────────────── *)

let run repo pr_num should_post provider_name =
  let tools = if should_post then
    [get_pr_info_tool; get_pr_diff_tool; post_review_tool]
  else
    [get_pr_info_tool; get_pr_diff_tool]
  in
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let provider_config = resolve_provider provider_name in
  let model_id = match provider_name with
    | "anthropic" -> "claude-sonnet-4-6"
    | _ -> "qwen3.5"
  in
  let config = {
    default_config with
    name = "review-agent";
    system_prompt = Some system_prompt;
    max_turns = 5;
    model = model_id;
  } in
  let options = { Agent.default_options with provider = Some provider_config } in
  let agent = Agent.create ~net ~config ~tools ~options () in
  let prompt = Printf.sprintf
    "Review PR #%s in repository %s. Get the PR info and diff, then provide your analysis."
    pr_num repo in
  match Agent.run ~sw agent prompt with
  | Ok response ->
    List.iter (function
      | Text t -> print_string t; print_newline ()
      | _ -> ()
    ) response.content
  | Error e ->
    Printf.eprintf "Error: %s\n" (Error.to_string e);
    exit 1

let () =
  let open Cmdliner in
  let repo =
    Arg.(required & opt (some string) None &
         info ["repo"; "r"] ~docv:"OWNER/REPO" ~doc:"GitHub repository (owner/repo)")
  in
  let pr_num =
    Arg.(required & opt (some string) None &
         info ["pr"; "p"] ~docv:"NUMBER" ~doc:"Pull request number")
  in
  let post =
    Arg.(value & flag & info ["post"] ~doc:"Post review as PR comment")
  in
  let provider =
    Arg.(value & opt string "local" &
         info ["provider"] ~docv:"NAME" ~doc:"LLM provider: local, anthropic, openrouter")
  in
  let cmd =
    Cmd.v (Cmd.info "oas-review" ~doc:"AI-powered PR code review"
             ~version:"0.46.0")
      Term.(const run $ repo $ pr_num $ post $ provider)
  in
  exit (Cmd.eval cmd)
