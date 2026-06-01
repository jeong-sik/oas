(** OpenAI-compatible response parsing.

    @since 0.92.0 extracted from Backend_chat_completions_v1

    @stability Internal
    @since 0.93.1 *)

val strip_json_markdown_fences : string -> string
val usage_of_chat_completions_v1_json : Yojson.Safe.t -> Types.api_usage option

(** Parse an OpenAI-compatible JSON response.
    Returns [Ok api_response] on success, [Error msg] on API error. *)
val parse_chat_completions_v1_response_result
  :  string
  -> (Types.api_response, string) result
