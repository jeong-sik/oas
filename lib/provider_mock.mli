(** Mock provider for testing -- eliminates network dependency.

    Supplies pre-programmed API responses in order.  When the response
    list is exhausted the mock cycles back to the beginning. *)

(** {1 Types} *)

(** A response function: given the conversation so far, produce a response. *)
type response_fn = Types.message list -> Types.api_response

(** Scripted mock: list of response functions applied in order. *)
type t

(** {1 Lifecycle} *)

val create : responses:response_fn list -> unit -> t
val next_response : t -> Types.message list -> (Types.api_response, Error.sdk_error) result
val reset : t -> unit
val call_count : t -> int

(** {1 Convenience builders} *)

val text_response :
  ?id:string -> ?model:string -> string -> response_fn

val tool_use_response :
  ?id:string -> ?model:string ->
  tool_name:string -> tool_input:Yojson.Safe.t ->
  unit -> response_fn

val tool_then_text :
  tool_name:string -> tool_input:Yojson.Safe.t ->
  final_text:string -> unit -> response_fn list

val thinking_response :
  ?id:string -> ?model:string ->
  thinking:string -> text:string ->
  unit -> response_fn

(** {1 Provider integration} *)

(** Return a {!Provider.config} suitable for identification purposes.
    Actual interception happens in {!Harness.run_case}. *)
val to_provider_config : unit -> Provider.config
