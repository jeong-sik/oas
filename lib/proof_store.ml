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

let ensure_dir path =
  if not (Sys.file_exists path) then
    Sys.mkdir path 0o755

let rec ensure_dir_rec path =
  if not (Sys.file_exists path) then begin
    ensure_dir_rec (Filename.dirname path);
    Sys.mkdir path 0o755
  end

let init_run config ~run_id =
  ensure_dir_rec (run_dir config ~run_id);
  ensure_dir (traces_dir config ~run_id);
  ensure_dir (evidence_dir config ~run_id)

let write_json path json =
  let content = Yojson.Safe.pretty_to_string json in
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_string oc content;
    output_char oc '\n')

let write_manifest config ~run_id proof =
  let path = manifest_path config ~run_id in
  write_json path (Cdal_proof.to_json proof)

let write_contract config ~run_id contract =
  let path = contract_path config ~run_id in
  write_json path (Risk_contract.to_yojson contract)

let append_tool_trace config ~run_id ~trace_id json =
  let path = Filename.concat (traces_dir config ~run_id)
      (trace_id ^ ".jsonl") in
  let oc = open_out_gen [Open_append; Open_creat; Open_wronly] 0o644 path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_string oc (Yojson.Safe.to_string json);
    output_char oc '\n')

let write_evidence config ~run_id ~ref_id json =
  let path = Filename.concat (evidence_dir config ~run_id)
      (ref_id ^ ".json") in
  write_json path json

let make_ref ~run_id ~subpath =
  Printf.sprintf "proof-store://%s/%s" run_id subpath
