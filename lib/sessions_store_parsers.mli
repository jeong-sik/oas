(** Pure JSON decoders for session store artifacts.

    Stateless parsing functions bridging raw JSON to typed
    {!Sessions_types} records.  No file I/O or store access. *)

(** {1 Generic JSON parsing} *)

val parse_json_string :
  string -> (Yojson.Safe.t, Error.sdk_error) result

val parse_runtime_json :
  (Yojson.Safe.t -> ('a, string) result) ->
  string -> ('a, Error.sdk_error) result

val decode_json_with :
  (Yojson.Safe.t -> 'a) ->
  string -> ('a, Error.sdk_error) result

(** {1 Domain decoders} *)

val shell_constraints_of_json : Yojson.Safe.t -> Tool.shell_constraints option

val tool_contract_of_json : Yojson.Safe.t -> Sessions_types.tool_contract

val telemetry_of_json : Yojson.Safe.t -> Sessions_types.telemetry

val structured_telemetry_of_json :
  Yojson.Safe.t -> Sessions_types.structured_telemetry

val evidence_of_json : Yojson.Safe.t -> Sessions_types.evidence

(** {1 Helpers} *)

val contains_substring : sub:string -> string -> bool

val workdir_policy_of_string : string -> Tool.workdir_policy option

val infer_event_name_from_kind : string -> string
