open Base
(** Anthropic Claude API request building and response parsing.

    @stability Internal
    @since 0.93.1 *)

(** Parse Anthropic API response JSON. *)
val parse_response : Yojson.Safe.t -> Types.api_response

(** Build request body assoc list for Anthropic Messages API. *)
val build_body_assoc
  :  config:Types.agent_state
  -> messages:Types.message list
  -> ?message_to_json:(Types.message -> Yojson.Safe.t)
  -> ?tools:Yojson.Safe.t list
  -> stream:bool
  -> unit
  -> (string * Yojson.Safe.t) list
