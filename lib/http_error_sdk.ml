(** Shared [Http_client.http_error] -> [Error.sdk_error] mapping.

    Keep this adapter above [llm_provider]: the provider layer deliberately
    does not depend on the SDK-wide [Error.sdk_error] type. *)

module Http_client = Llm_provider.Http_client
module Retry = Llm_provider.Retry

type accept_rejected =
  | Api_invalid_request
  | Config_invalid_config of { field : string }

let invalid_request reason =
  Error.Api
    (Retry.InvalidRequest { message = reason; reason = Retry.Unknown_invalid_request })
;;

let of_http_error ?(accept_rejected = Api_invalid_request) (err : Http_client.http_error)
  : Error.sdk_error
  =
  match err with
  | Http_client.HttpError { code; body } ->
    Error.Api (Retry.classify_error ~status:code ~body)
  | Http_client.NetworkError { message; kind; _ } ->
    Error.Api (Retry.NetworkError { message; kind })
  | Http_client.TimeoutError _ -> Error.Provider (Llm_provider.Error.of_http_error err)
  | Http_client.AcceptRejected { reason } ->
    (match accept_rejected with
     | Api_invalid_request -> invalid_request reason
     | Config_invalid_config { field } ->
       Error.Config (Error.InvalidConfig { field; detail = reason }))
  | Http_client.ProviderTerminal { kind = Http_client.Max_turns r; _ } ->
    Error.Agent (Error.MaxTurnsExceeded { turns = r.turns; limit = r.limit })
  | Http_client.ProviderTerminal { kind = Http_client.Other _; _ }
  | Http_client.ProviderFailure _ -> Error.Provider (Llm_provider.Error.of_http_error err)
;;
