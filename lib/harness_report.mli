(** Structured reports for harness runs.

    @stability Evolving
    @since 0.93.1 *)

type case_status =
  | Pass
  | Fail
  | Skip

type case_result =
  { case_id : string
  ; kind : Harness_case.kind
  ; status : case_status
  ; verdicts : Harness.verdict list
  ; evidence : string list
  ; detail : string option
  ; response_text : string option
  ; raw_trace_path : string option
  ; metrics : Eval.run_metrics option
  }

type summary =
  { total : int
  ; passed : int
  ; failed : int
  ; skipped : int
  ; evaluated_runs : int
  ; skipped_runs : int
  ; pass_rate : float
  }

type t =
  { results : case_result list
  ; summary : summary
  }

val summarize : case_result list -> summary
val of_results : case_result list -> t
val to_json : t -> Yojson.Safe.t
val to_markdown : t -> string
val to_junit_xml : t -> string
