(** Typed telemetry events for the OAS inference layer. *)

type timeout_type =
  | No_response
  | Ttft_exceeded
[@@deriving yojson, show]

type t =
  | Streaming_first_chunk of {
      provider : string;
      model : string;
      ttfrc_ms : float;
      requested_at : float;
    }
  | Streaming_chunk_n of {
      provider : string;
      model : string;
      chunk_index : int;
      inter_chunk_ms : float;
    }
  | Thinking_complete of {
      provider : string;
      model : string;
      thinking_duration_ms : float;
    }
  | Timeout of {
      provider : string;
      model : string;
      timeout_type : timeout_type;
    }
  | Prefill_complete of {
      provider : string;
      model : string;
      prompt_eval_tokens : int;
      prompt_eval_ms : float;
      cache_hit : bool;
    }
  | Budget_exceeded of {
      agent_name : string;
      run_id : string;
      spent_usd : float;
      limit_usd : float;
    }
[@@deriving yojson, show]

val event_type_name : t -> string
(** Human-readable event type label for metrics and logging. *)
