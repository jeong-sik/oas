(** Local filesystem artifact store for CDAL proof bundles.

    Part of the Contract-Driven Agent Loop (CDAL) PoC-1.
    Stores proof manifests, contract snapshots, tool traces,
    and evidence artifacts under [{root}/proofs/{run_id}/]. *)

type config = {
  root: string;  (** Default: [~/.oas] *)
}

val default_config : config

(** Create directory structure for a new run. *)
val init_run : config -> run_id:string -> unit

(** Write the 15-field proof manifest. *)
val write_manifest : config -> run_id:string -> Cdal_proof.t -> unit

(** Write the frozen contract snapshot. *)
val write_contract : config -> run_id:string -> Risk_contract.t -> unit

(** Append a single tool trace entry (JSONL). *)
val append_tool_trace : config -> run_id:string -> trace_id:string ->
  Yojson.Safe.t -> unit

(** Write a raw evidence artifact. *)
val write_evidence : config -> run_id:string -> ref_id:string ->
  Yojson.Safe.t -> unit

(** Construct a [proof-store://] artifact reference. *)
val make_ref : run_id:string -> subpath:string -> Cdal_proof.artifact_ref

(** Path to manifest.json for a run. *)
val manifest_path : config -> run_id:string -> string
