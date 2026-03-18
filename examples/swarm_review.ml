(** Swarm PR review — 3-agent Supervisor mode demo.

    Demonstrates:
    - Swarm Engine (Layer 2) with Supervisor orchestration
    - 3 agents: reader (gets diff), reviewer (finds issues), supervisor (synthesizes)
    - Convergence with metric callback
    - Zero-LLM testable via closure-based agent_entry

    Prerequisites:
    - gh CLI authenticated
    - A running LLM (llama-server :8085 or set LLM_BASE_URL)

    Usage:
      dune exec examples/swarm_review.exe -- jeong-sik/oas 123 *)

open Agent_sdk
open Agent_sdk_swarm
open Types

(* ── Shell helper ──────────────────────────────────────────── *)

let run_command cmd =
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 4096 in
  (try while true do
     Buffer.add_string buf (input_line ic);
     Buffer.add_char buf '\n'
   done with End_of_file -> ());
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Ok (Buffer.contents buf)
  | Unix.WEXITED n -> Error (Printf.sprintf "exit %d" n)
  | _ -> Error "killed"

(* ── Provider config ───────────────────────────────────────── *)

let make_provider () =
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

(* ── Agent factory ─────────────────────────────────────────── *)

let make_agent ~net ~name ~system_prompt ~tools =
  let provider = make_provider () in
  Agent.create ~net
    ~config:{ default_config with
      name;
      system_prompt = Some system_prompt;
      max_turns = 3;
      model = "qwen3.5";
    }
    ~options:{ Agent.default_options with provider = Some provider }
    ~tools ()

(* ── Tools ─────────────────────────────────────────────────── *)

let get_diff_tool repo pr_num =
  Tool.create ~name:"get_diff"
    ~description:"Get the unified diff of the PR"
    ~parameters:[]
    (fun _args ->
      let cmd = Printf.sprintf "gh pr diff %s --repo %s" (Filename.quote pr_num) (Filename.quote repo) in
      match run_command cmd with
      | Ok output ->
        let max_len = 8000 in
        let trimmed = if String.length output > max_len then
          String.sub output 0 max_len ^ "\n...[truncated]"
        else output in
        Ok { content = trimmed }
      | Error msg -> Error { message = msg; recoverable = true })

let get_files_tool repo pr_num =
  Tool.create ~name:"get_files"
    ~description:"List files changed in the PR"
    ~parameters:[]
    (fun _args ->
      let cmd = Printf.sprintf "gh pr view %s --repo %s --json files --jq '.files[].path'" (Filename.quote pr_num) (Filename.quote repo) in
      match run_command cmd with
      | Ok output -> Ok { content = output }
      | Error msg -> Error { message = msg; recoverable = true })

(* ── Main ──────────────────────────────────────────────────── *)

let () =
  let args = Sys.argv in
  if Array.length args < 3 then begin
    Printf.eprintf "Usage: swarm_review.exe <owner/repo> <pr_number>\n";
    exit 1
  end;
  let repo = args.(1) in
  let pr_num = args.(2) in
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->

  (* 1. Reader agent — gathers PR data *)
  let reader_agent = make_agent ~net ~name:"reader"
    ~system_prompt:"You read PR diffs and file lists. Summarize the changes concisely: what files changed, what the diff does, key patterns."
    ~tools:[get_diff_tool repo pr_num; get_files_tool repo pr_num] in

  (* 2. Reviewer agent — finds issues *)
  let reviewer_agent = make_agent ~net ~name:"reviewer"
    ~system_prompt:{|You are a code reviewer. Given a PR summary, identify:
- Bugs or logic errors (HIGH)
- Missing error handling (MEDIUM)
- Type safety issues (MEDIUM)
- Performance concerns (LOW)
Output as a numbered list with severity tags.|}
    ~tools:[] in

  (* 3. Supervisor agent — synthesizes final verdict *)
  let supervisor_agent = make_agent ~net ~name:"supervisor"
    ~system_prompt:{|You are a review supervisor. Given worker outputs (a PR summary and review findings), produce a final verdict:
## Review Summary
**Verdict**: LGTM | Minor Issues | Needs Changes
### Key Findings
(numbered list from the reviewer, deduplicated and prioritized)
### Recommendation
(1-2 sentences)|}
    ~tools:[] in

  (* Build swarm entries *)
  let entries = [
    Swarm_types.make_entry ~name:"supervisor" ~role:Summarize ~clock supervisor_agent;
    Swarm_types.make_entry ~name:"reader" ~role:Discover ~clock reader_agent;
    Swarm_types.make_entry ~name:"reviewer" ~role:Verify ~clock reviewer_agent;
  ] in

  let config = Swarm_types.{
    entries;
    mode = Supervisor;
    convergence = None;
    max_parallel = 2;
    prompt = Printf.sprintf "Review PR #%s in %s" pr_num repo;
    timeout_sec = Some 120.0;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
  } in

  let callbacks = Swarm_types.{
    no_callbacks with
    on_agent_start = Some (fun name ->
      Printf.eprintf "[swarm] agent %s started\n%!" name);
    on_agent_done = Some (fun name status ->
      let tag = match status with
        | Done_ok _ -> "ok"
        | Done_error _ -> "error"
        | _ -> "?" in
      Printf.eprintf "[swarm] agent %s finished (%s)\n%!" name tag);
  } in

  match Runner.run ~sw ~clock ~callbacks config with
  | Ok result ->
    Printf.eprintf "[swarm] completed in %.1fs (%d iteration(s), converged=%b)\n%!"
      result.total_elapsed (List.length result.iterations) result.converged;
    (* Print supervisor output (last agent in Supervisor mode) *)
    List.iter (fun (iter : Swarm_types.iteration_record) ->
      List.iter (fun (name, status) ->
        match status with
        | Swarm_types.Done_ok { text; _ } when name = "supervisor" ->
          print_string text; print_newline ()
        | _ -> ()
      ) iter.agent_results
    ) result.iterations
  | Error e ->
    Printf.eprintf "Swarm error: %s\n" (Error.to_string e);
    exit 1
