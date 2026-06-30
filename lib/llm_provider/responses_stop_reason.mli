(** Single source of truth for OpenAI Responses terminal status ->
    {!Types.stop_reason} mapping.

    Responses exposes terminal state through [status] plus optional
    [incomplete_details.reason] / [error.message]. Both the non-streaming
    Responses parser and the Responses SSE terminal event handler must use this
    module so cut-off or failed responses win over partial tool-call items. *)

val of_status
  :  status:string option
  -> incomplete_reason:string option
  -> failed_message:string option
  -> has_tool_calls:bool
  -> Types.stop_reason
