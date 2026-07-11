(** LLM judgment boundary for repeated typed tool failures.

    The main agent loop supplies only structural failure episodes to the judge:
    no hidden reasoning, repeated narration, provider name, or model name is
    replayed. The judge returns a closed decision that is validated against the
    episodes before it can affect execution.

    @stability Evolving
    @since 0.212.0 *)

type revised_call =
  { current_tool_use_id : string
  ; tool_name : string
  ; revised_input : Yojson.Safe.t
  }
[@@deriving show]

type decision = private
  | Retry_modified of revised_call list
  | Replan of { instruction : string }
  | Ask_user of
      { question : string
      ; schema : Yojson.Safe.t option
      }
  | Defer of { reason : string }
  (** End the current agent run and return control to the caller. [reason] is
        observation-only model output: it is neither a wake condition nor a
        scheduling instruction. The embedding host owns any later activity. *)
[@@deriving show]

(** Observation-only JSON projection. Parsing remains private so external code
    cannot bypass episode validation to construct a [decision]. *)
val decision_to_yojson : decision -> Yojson.Safe.t

(** External-observability projection of a decision. Reports only the closed
    action and non-sensitive cardinality/tool-name metadata; revised inputs,
    instructions, questions, schemas, and defer reasons remain internal.
    @since 0.212.0 *)
val decision_observation_to_yojson : decision -> Yojson.Safe.t

type model_request =
  { system_prompt : string
  ; user_prompt : string
  ; output_schema : Yojson.Safe.t
  }

(** A caller-owned LLM completion boundary. Coordinators may implement this
    with their runtime catalog and provider fallback without coupling OAS to
    that coordinator. The callback must return only the model's response text;
    OAS owns schema parsing and decision validation. *)
type completion = sw:Eio.Switch.t -> model_request -> (string, Error.sdk_error) result

type judge

val create : complete:completion -> judge

type decision_error =
  | Empty_revised_calls
  | Duplicate_revised_call of { current_tool_use_id : string }
  | Missing_revised_call of { current_tool_use_id : string }
  | Unexpected_revised_call of { current_tool_use_id : string }
  | Revised_tool_name_mismatch of
      { current_tool_use_id : string
      ; expected : string
      ; got : string
      }
  | Unchanged_revised_input of { current_tool_use_id : string }
  | Blank_replan_instruction
  | Blank_question
  | Blank_defer_reason
[@@deriving show]

type response_error =
  | Invalid_json of string
  | Expected_object
  | Missing_field of string
  | Invalid_field of string
  | Duplicate_field of string
  | Unexpected_field of string
  | Unsupported_action of string
  | Invalid_decision of decision_error
[@@deriving show]

type judge_error =
  | Completion_failed of Error.sdk_error
  | Completion_raised of string
  | Invalid_response of response_error

val judge_error_to_string : judge_error -> string

(** Invoke the configured LLM once, parse its exact JSON object, and validate
    the closed decision against every supplied episode. Empty episode lists are
    rejected before the callback is invoked. *)
val decide
  :  sw:Eio.Switch.t
  -> agent_name:string
  -> turn:int
  -> episodes:Tool_failure_episode.t list
  -> judge
  -> (decision, judge_error) result

(** Durable receipt stored only in OAS message metadata/checkpoints. Provider
    request serializers do not forward generic message metadata. *)
type receipt

val make_receipt
  :  resume_turn:int
  -> decided_at:float
  -> episodes:Tool_failure_episode.t list
  -> decision:decision
  -> receipt

val receipt_resume_turn : receipt -> int
val receipt_decided_at : receipt -> float
val receipt_decision : receipt -> decision
val receipt_tool_names : receipt -> string list

type receipt_error =
  | Result_message_not_found
  | Duplicate_receipt_metadata
  | Invalid_receipt_metadata of string
  | Run_boundary_error of Tool_failure_episode.error
  | Receipt_episode_mismatch
  | Receipt_decision_invalid of decision_error
[@@deriving show]

(** Add a versioned receipt to the latest-run result message containing every
    current episode id. Existing metadata is preserved; a duplicate receipt is
    an explicit error. *)
val attach_receipt
  :  messages:Types.message list
  -> episodes:Tool_failure_episode.t list
  -> receipt:receipt
  -> (Types.message list, receipt_error) result

(** Read a receipt only from the latest-run message containing a [ToolResult].
    An older receipt is never reused after a run or tool boundary. *)
val latest_receipt : Types.message list -> (receipt option, receipt_error) result

(** Validate a restored receipt against reconstructed adjacent episodes. *)
val validate_receipt
  :  episodes:Tool_failure_episode.t list
  -> receipt
  -> (unit, receipt_error) result

(** Render a one-turn system-prompt suffix. [Ask_user] and [Defer] are
    terminal control decisions and therefore have no continuation context. *)
val system_context : receipt -> string option
