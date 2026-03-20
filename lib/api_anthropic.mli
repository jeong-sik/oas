(** Anthropic Claude API request building and response parsing. *)

(** Parse Anthropic API response JSON. *)
val parse_response : Yojson.Safe.t -> Types.api_response

(** Build request body assoc list for Anthropic Messages API. *)
val build_body_assoc :
  config:Types.agent_state ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  stream:bool ->
  unit -> (string * Yojson.Safe.t) list
