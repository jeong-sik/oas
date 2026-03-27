type trace_entry = {
  ts: float;
  tool_name: string;
  input: Yojson.Safe.t;
  output: string option;
  error: string option;
  duration_ms: int option;
}

type state = {
  run_id: string;
  store: Proof_store.config;
  contract: Risk_contract.t;
  mode_decision: Mode_resolver.decision;
  capability_snapshot: Cdal_proof.capability_snapshot;
  mutable started_at: float;
  mutable ended_at: float;
  mutable provider_snapshot: Cdal_proof.provider_snapshot;
  mutable pending_tool: (float * string * Yojson.Safe.t) option;
  mutable trace_count: int;
}

let generate_run_id () =
  let now = Unix.gettimeofday () in
  let rand = Random.bits () land 0xFFFFFF in
  Printf.sprintf "cdal-%d-%06x" (int_of_float (now *. 1000.0)) rand

let create ~store ~contract ~mode_decision ~capability_snapshot =
  let run_id = generate_run_id () in
  Proof_store.init_run store ~run_id;
  {
    run_id;
    store;
    contract;
    mode_decision;
    capability_snapshot;
    started_at = 0.0;
    ended_at = 0.0;
    provider_snapshot = {
      provider_name = "unknown";
      model_id = "unknown";
      api_version = None;
    };
    pending_tool = None;
    trace_count = 0;
  }

let run_id st = st.run_id

let flush_trace st entry =
  st.trace_count <- st.trace_count + 1;
  let trace_id = Printf.sprintf "trace-%04d" st.trace_count in
  let json = `Assoc [
    "ts", `Float entry.ts;
    "tool_name", `String entry.tool_name;
    "input", entry.input;
    "output", (match entry.output with
               | Some s -> `String s | None -> `Null);
    "error", (match entry.error with
              | Some s -> `String s | None -> `Null);
    "duration_ms", (match entry.duration_ms with
                    | Some d -> `Int d | None -> `Null);
  ] in
  Proof_store.append_tool_trace st.store ~run_id:st.run_id ~trace_id json

let complete_pending_tool st ~output ~error =
  match st.pending_tool with
  | None -> ()
  | Some (start_ts, tool_name, input) ->
    let now = Unix.gettimeofday () in
    let duration_ms = int_of_float ((now -. start_ts) *. 1000.0) in
    flush_trace st {
      ts = start_ts; tool_name; input;
      output; error; duration_ms = Some duration_ms;
    };
    st.pending_tool <- None

let hooks st =
  let open Hooks in
  {
    before_turn = Some (fun event ->
      (match event with
       | BeforeTurn { turn = 1; _ } ->
         st.started_at <- Unix.gettimeofday ()
       | BeforeTurn _ -> ()
       | _ -> ());
      Continue);

    before_turn_params = None;

    after_turn = Some (fun event ->
      (match event with
       | AfterTurn { response; _ } ->
         st.provider_snapshot <- {
           provider_name = (match st.provider_snapshot.provider_name with
                            | "unknown" -> "detected"
                            | p -> p);
           model_id = response.Types.model;
           api_version = None;
         }
       | _ -> ());
      Continue);

    pre_tool_use = Some (fun event ->
      (match event with
       | PreToolUse { tool_name; input; _ } ->
         complete_pending_tool st ~output:None ~error:None;
         st.pending_tool <- Some (Unix.gettimeofday (), tool_name, input)
       | _ -> ());
      Continue);

    post_tool_use = Some (fun event ->
      (match event with
       | PostToolUse { output; _ } ->
         let output_str, error_str = match output with
           | Ok { Types.content; _ } -> (Some content, None)
           | Error { Types.message; _ } -> (None, Some message)
         in
         complete_pending_tool st ~output:output_str ~error:error_str
       | _ -> ());
      Continue);

    post_tool_use_failure = Some (fun event ->
      (match event with
       | PostToolUseFailure { error; _ } ->
         complete_pending_tool st ~output:None ~error:(Some error)
       | _ -> ());
      Continue);

    on_stop = Some (fun event ->
      (match event with
       | OnStop _ -> st.ended_at <- Unix.gettimeofday ()
       | _ -> ());
      Continue);

    on_idle = None;

    on_error = Some (fun event ->
      (match event with
       | OnError _ -> st.ended_at <- Unix.gettimeofday ()
       | _ -> ());
      Continue);

    on_tool_error = None;
    pre_compact = None;
  }

let finalize st ~result_status =
  complete_pending_tool st ~output:None ~error:None;
  if st.ended_at = 0.0 then
    st.ended_at <- Unix.gettimeofday ();
  let tool_trace_refs =
    List.init st.trace_count (fun i ->
      Proof_store.make_ref ~run_id:st.run_id
        ~subpath:(Printf.sprintf "tool_traces/trace-%04d.jsonl" (i + 1)))
  in
  let proof : Cdal_proof.t = {
    schema_version = Cdal_proof.schema_version_current;
    run_id = st.run_id;
    contract_id = Risk_contract.contract_id st.contract;
    requested_execution_mode =
      st.contract.runtime_constraints.requested_execution_mode;
    effective_execution_mode = st.mode_decision.effective_mode;
    mode_decision_source = st.mode_decision.source;
    risk_class = st.contract.runtime_constraints.risk_class;
    provider_snapshot = st.provider_snapshot;
    capability_snapshot = st.capability_snapshot;
    tool_trace_refs;
    raw_evidence_refs = [];
    checkpoint_ref = None;
    result_status;
    started_at = st.started_at;
    ended_at = st.ended_at;
  } in
  Proof_store.write_manifest st.store ~run_id:st.run_id proof;
  Proof_store.write_contract st.store ~run_id:st.run_id st.contract;
  proof
