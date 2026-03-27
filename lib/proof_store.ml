type config = {
  root: string;
}

let default_config =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  { root = Filename.concat home ".oas" }

let proofs_dir config = Filename.concat config.root "proofs"
let run_dir config ~run_id = Filename.concat (proofs_dir config) run_id
let traces_dir config ~run_id = Filename.concat (run_dir config ~run_id) "tool_traces"
let evidence_dir config ~run_id = Filename.concat (run_dir config ~run_id) "evidence"

let manifest_path config ~run_id =
  Filename.concat (run_dir config ~run_id) "manifest.json"

let contract_path config ~run_id =
  Filename.concat (run_dir config ~run_id) "contract.json"

let ignore_error = function
  | Ok () -> ()
  | Error _ -> ()

let init_run config ~run_id =
  ignore_error (Fs_result.ensure_dir (traces_dir config ~run_id));
  ignore_error (Fs_result.ensure_dir (evidence_dir config ~run_id))

let write_json path json =
  let content = Yojson.Safe.pretty_to_string json ^ "\n" in
  ignore_error (Fs_result.write_file path content)

let write_manifest config ~run_id proof =
  write_json (manifest_path config ~run_id) (Cdal_proof.to_json proof)

let write_contract config ~run_id contract =
  write_json (contract_path config ~run_id) (Risk_contract.to_yojson contract)

let append_tool_trace config ~run_id ~trace_id json =
  let path = Filename.concat (traces_dir config ~run_id)
      (trace_id ^ ".jsonl") in
  let line = Yojson.Safe.to_string json ^ "\n" in
  ignore_error (Fs_result.append_file path line)

let write_evidence config ~run_id ~ref_id json =
  let path = Filename.concat (evidence_dir config ~run_id)
      (ref_id ^ ".json") in
  write_json path json

let make_ref ~run_id ~subpath =
  Printf.sprintf "proof-store://%s/%s" run_id subpath
