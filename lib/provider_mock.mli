(** Mock provider for testing — eliminates network dependency.

    Supplies pre-programmed API responses in order.  When the response
    list is exhausted the mock cycles back to the beginning. *)

(** {1 Types} *)

(** A response function: given the conversation so far, produce a response. *)
type response_fn = Types.message list -> Types.api_response

(** Scripted mock state. *)
type t

(** {1 Lifecycle} *)

(** Create a mock from a list of response functions. *)
val create : responses:response_fn list -> unit -> t

(** Get the next response for the given messages.
    Cycles when the list is exhausted. *)
val next_response :
  t -> Types.message list -> (Types.api_response, Error.sdk_error) result

(** Reset the mock index to replay from the beginning. *)
val reset : t -> unit

(** How many responses have been consumed so far. *)
val call_count : t -> int

(** {1 Convenience Builders} *)

(** Build a simple text response function. *)
val text_response :
  ?id:string -> ?model:string -> string -> response_fn

(** Build a tool-use response function. *)
val tool_use_response :
  ?id:string -> ?model:string ->
  tool_name:string -> tool_input:Yojson.Safe.t ->
  unit -> response_fn

(** Build a [tool_use, text] response pair for two-step flows. *)
val tool_then_text :
  tool_name:string -> tool_input:Yojson.Safe.t ->
  final_text:string -> unit -> response_fn list

(** Build a thinking + text response function. *)
val thinking_response :
  ?id:string -> ?model:string ->
  thinking:string -> text:string ->
  unit -> response_fn

(** Create a {!Provider.config} identifying this mock provider. *)
val to_provider_config : unit -> Provider.config
