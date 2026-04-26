(** A2A Client — client for Agent-to-Agent protocol.

    @since 0.55.0 *)

(* ── Types ────────────────────────────────────────────────────── *)

type remote_agent = {
  endpoint: string;
  agent_card: Agent_card.agent_card;
}

(* ── HTTP helpers ─────────────────────────────────────────────── *)

let http_get ~sw ~net url =
  match Llm_provider.Http_client.get_sync ~sw ~net ~url ~headers:[] () with
  | Ok (code, body) when code >= 200 && code < 300 -> Ok body
  | Ok (code, body) ->
    Error (Error.Orchestration
      (DiscoveryFailed { url; detail = Printf.sprintf "HTTP %d: %s" code body }))
  | Error (Llm_provider.Http_client.HttpError { code; body }) ->
    Error (Error.Orchestration
      (DiscoveryFailed { url; detail = Printf.sprintf "HTTP %d: %s" code body }))
  | Error (Llm_provider.Http_client.AcceptRejected { reason }) ->
    Error (Error.Orchestration (DiscoveryFailed { url; detail = reason }))
  | Error (Llm_provider.Http_client.NetworkError { message; _ }) ->
    Error (Error.Orchestration (DiscoveryFailed { url; detail = message }))
  | Error (Llm_provider.Http_client.CliTransportRequired { kind }) ->
    (* [get_sync] never constructs this variant; only [Complete.complete]
       does, as a wiring guard.  Treat defensively as a network-style
       discovery failure so the match stays exhaustive. *)
    Error (Error.Orchestration (DiscoveryFailed {
      url;
      detail = Printf.sprintf "CLI transport required for %s" kind }))
  | Error (Llm_provider.Http_client.ProviderTerminal { message; _ }) ->
    (* Same defensive note as [CliTransportRequired]: pure HTTP discovery
       cannot produce a CLI subprocess terminal condition; reduce to
       message text so the exhaustive match stays sound. *)
    Error (Error.Orchestration (DiscoveryFailed { url; detail = message }))

let http_post ~sw ~net ~url ~body =
  let headers = [("Content-Type", "application/json")] in
  match Llm_provider.Http_client.post_sync ~sw ~net ~url ~headers ~body () with
  | Ok (code, resp_body) when code >= 200 && code < 300 -> Ok resp_body
  | Ok (code, resp_body) ->
    Error (Error.A2a (ProtocolError {
      detail = Printf.sprintf "HTTP %d: %s" code resp_body }))
  | Error (Llm_provider.Http_client.HttpError { code; body = err_body }) ->
    Error (Error.A2a (ProtocolError {
      detail = Printf.sprintf "HTTP %d: %s" code err_body }))
  | Error (Llm_provider.Http_client.AcceptRejected { reason }) ->
    Error (Error.A2a (ProtocolError { detail = reason }))
  | Error (Llm_provider.Http_client.NetworkError { message; _ }) ->
    Error (Error.A2a (ProtocolError { detail = message }))
  | Error (Llm_provider.Http_client.CliTransportRequired { kind }) ->
    Error (Error.A2a (ProtocolError {
      detail = Printf.sprintf "CLI transport required for %s" kind }))
  | Error (Llm_provider.Http_client.ProviderTerminal { message; _ }) ->
    (* Pure HTTP RPC cannot produce a CLI subprocess terminal condition;
       defensive handling mirrors [http_get] so the exhaustive match
       stays sound and the message survives for diagnostics. *)
    Error (Error.A2a (ProtocolError { detail = message }))

(* ── JSON-RPC ─────────────────────────────────────────────────── *)

let next_id = Atomic.make 1

let rpc_call ~sw ~net endpoint method_ params =
  let id = Atomic.fetch_and_add next_id 1 in
  let request = `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String method_);
    ("params", params);
    ("id", `Int id);
  ] in
  let url = endpoint ^ "/a2a" in
  let body = Yojson.Safe.to_string request in
  match http_post ~sw ~net ~url ~body with
  | Error _ as e -> e
  | Ok response_body ->
    (try
       let json = Yojson.Safe.from_string response_body in
       let open Yojson.Safe.Util in
       match json |> member "error" with
       | `Null ->
         Ok (json |> member "result")
       | err ->
         let msg =
           match err |> member "message" |> to_string_option with
           | Some s -> s
           | None -> Yojson.Safe.to_string err
         in
         Error (Error.A2a (ProtocolError { detail = msg }))
     with
     | Yojson.Json_error e ->
       Error (Error.A2a (ProtocolError { detail = "JSON parse: " ^ e }))
     | Yojson.Safe.Util.Type_error (e, _) ->
       Error (Error.A2a (ProtocolError { detail = "JSON type: " ^ e })))

(* ── Public API ───────────────────────────────────────────────── *)

let discover ~sw ~net url =
  let card_url = url ^ "/.well-known/agent.json" in
  match http_get ~sw ~net card_url with
  | Error _ as e -> e
  | Ok body ->
    (try
       let json = Yojson.Safe.from_string body in
       match Agent_card.of_json json with
       | Ok card -> Ok { endpoint = url; agent_card = card }
       | Error e ->
         Error (Error.Orchestration
           (DiscoveryFailed { url; detail = "invalid agent card: " ^ Error.to_string e }))
     with Yojson.Json_error e ->
       Error (Error.Orchestration
         (DiscoveryFailed { url; detail = "JSON parse: " ^ e })))

let send_task ~sw ~net remote message =
  let params = `Assoc [
    ("message", A2a_task.task_message_to_yojson message)
  ] in
  match rpc_call ~sw ~net remote.endpoint "tasks/send" params with
  | Error _ as e -> e
  | Ok result ->
    match A2a_task.task_of_yojson result with
    | Ok task -> Ok task
    | Error msg -> Error (Error.A2a (ProtocolError { detail = msg }))

let get_task ~sw ~net remote task_id =
  let params = `Assoc [("id", `String task_id)] in
  match rpc_call ~sw ~net remote.endpoint "tasks/get" params with
  | Error _ as e -> e
  | Ok result ->
    match A2a_task.task_of_yojson result with
    | Ok task -> Ok task
    | Error msg -> Error (Error.A2a (ProtocolError { detail = msg }))

let cancel_task ~sw ~net remote task_id =
  let params = `Assoc [("id", `String task_id)] in
  match rpc_call ~sw ~net remote.endpoint "tasks/cancel" params with
  | Error _ as e -> e
  | Ok result ->
    match A2a_task.task_of_yojson result with
    | Ok task -> Ok task
    | Error msg -> Error (Error.A2a (ProtocolError { detail = msg }))

let text_of_task (task : A2a_task.task) =
  List.filter_map (fun (msg : A2a_task.task_message) ->
    let texts = List.filter_map (function
      | A2a_task.Text_part s -> Some s
      | _ -> None
    ) msg.parts in
    match texts with [] -> None | _ -> Some (String.concat "\n" texts)
  ) task.messages
  |> String.concat "\n\n"
