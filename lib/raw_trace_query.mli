(** Read-side operations for raw traces: list runs, read, summarize,
    validate.

    Extracted from {!Raw_trace} to separate write-path (recording) from
    read-path (query/analysis).  All functions operate on JSONL files
    written by {!Raw_trace} and return structured results. *)

(** {1 Run discovery} *)

(** Extract distinct [run_ref] values from parsed records, ordered by
    [start_seq]. *)
val run_refs_of_records :
  path:string -> Raw_trace.record list -> Raw_trace.run_ref list

(** Read the JSONL file at [path] and return all discovered runs. *)
val read_runs :
  path:string -> unit -> (Raw_trace.run_ref list, Error.sdk_error) result

(** Read all records belonging to the given run. *)
val read_run :
  Raw_trace.run_ref -> (Raw_trace.record list, Error.sdk_error) result

(** {1 Summarize} *)

(** Produce a {!Raw_trace.run_summary} for the given run. *)
val summarize_run :
  Raw_trace.run_ref -> (Raw_trace.run_summary, Error.sdk_error) result

(** {1 Validate} *)

(** Validate structural integrity (sequence monotonicity, start/finish
    pairing, tool execution pairing) and return a
    {!Raw_trace.validation_result}. *)
val validate_run :
  Raw_trace.run_ref -> (Raw_trace.run_validation, Error.sdk_error) result
