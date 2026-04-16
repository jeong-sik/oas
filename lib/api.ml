(** API dispatch — re-exports provider modules and routes create_message *)

open Types

type response_accept = Types.api_response -> (unit, string) result

(* Re-export Api_common *)
let default_base_url = Api_common.default_base_url
let api_version = Api_common.api_version
let max_response_body = Api_common.max_response_body
let string_is_blank = Api_common.string_is_blank
let text_blocks_to_string = Api_common.text_blocks_to_string
let json_of_string_or_raw = Api_common.json_of_string_or_raw
let content_block_to_json = Api_common.content_block_to_json
let content_block_of_json = Api_common.content_block_of_json
let message_to_json = Api_common.message_to_json
let make_https = Api_common.make_https

(* Re-export Api_anthropic *)
let parse_response = Api_anthropic.parse_response
let build_body_assoc = Api_anthropic.build_body_assoc

(* Re-export Api_openai *)
let openai_messages_of_message = Api_openai.openai_messages_of_message
let openai_content_parts_of_blocks = Api_openai.openai_content_parts_of_blocks
let build_openai_body = Api_openai.build_openai_body
let parse_openai_response_result = Llm_provider.Backend_openai_parse.parse_openai_response_result


(* Wall-clock latency patch. Parser layers (api_anthropic and backend_ollama)
   leave request_latency_ms at 0 as a sentinel because they only see the JSON
   response body; only the transport layer measures wall time. Without this
   patch, downstream telemetry (dashboard latency panel, model_inference_metrics
   percentiles) treats every turn as zero-latency and filters it out. *)
let patch_latency (resp : Types.api_response) (latency_ms : int)
    : Types.api_response =
  let telemetry =
    match resp.telemetry with
    | Some t -> Some { t with Llm_provider.Types.request_latency_ms = latency_ms }
    | None ->
      Some
        { Llm_provider.Types.system_fingerprint = None;
          timings = None;
          reasoning_tokens = None;
          request_latency_ms = latency_ms;
          provider_kind = None;
          reasoning_effort = None;
          canonical_model_id = None;
          effective_context_window = None }
  in
  { resp with telemetry }

(** Send a non-streaming message to the API, dispatching by provider *)
let create_message ~sw ~net ?(base_url=default_base_url) ?provider ?clock ?retry_config ~config ~messages ?tools ?slot_id () =
  let resolve_result = match provider with
    | Some p ->
        (match Provider.resolve p with
         | Ok (url, _key, headers) -> Ok (p, url, headers)
         | Error e -> Error e)
    | None ->
        (match Sys.getenv_opt "ANTHROPIC_API_KEY" with
         | Some key ->
             let fallback_provider : Provider.config =
               {
                 provider = Provider.Anthropic;
                 model_id = model_to_string config.config.model;
                 api_key_env = "ANTHROPIC_API_KEY";
               }
             in
             Ok
               ( fallback_provider,
                 base_url,
                 [
                   ("Content-Type", "application/json");
                   ("x-api-key", key);
                   ("anthropic-version", api_version);
                 ] )
         | None -> Error (Error.Config (MissingEnvVar { var_name = "ANTHROPIC_API_KEY" })))
  in
  match resolve_result with
  | Error e -> Error e
  | Ok (provider_cfg, base_url, header_list) ->
  let headers = Http.Header.of_list header_list in
  let model_spec = Provider.model_spec_of_config provider_cfg in
  let kind = model_spec.request_kind in
  let path = model_spec.request_path in
  let body_str =
    match kind with
    | Provider.Anthropic_messages ->
        Yojson.Safe.to_string (`Assoc (build_body_assoc ~config ~messages ?tools ~stream:false ()))
    | Provider.Openai_chat_completions ->
        Api_openai.build_openai_body ~provider_config:provider_cfg ~config
          ~messages ?tools ?slot_id ()
    | Provider.Custom name ->
        (match Provider.find_provider name with
         | Some impl -> impl.build_body ~config ~messages ?tools ()
         | None -> Yojson.Safe.to_string (`Assoc []))
  in
  let uri = Uri.of_string (base_url ^ path) in

  let https = make_https () in
  let client = Cohttp_eio.Client.make ~https net in
  let do_request () =
    let t0 = Unix.gettimeofday () in
    let measured_latency_ms () =
      int_of_float ((Unix.gettimeofday () -. t0) *. 1000.0)
    in
    try
      let resp, body = Cohttp_eio.Client.post ~sw client ~headers ~body:(Cohttp_eio.Body.of_string body_str) uri in
      match Cohttp.Response.status resp with
      | `OK ->
          let body_str = Eio.Buf_read.(of_flow ~max_size:max_response_body body |> take_all) in
          let lat = measured_latency_ms () in
          (match kind with
           | Provider.Anthropic_messages ->
               Ok
                 (parse_response (Yojson.Safe.from_string body_str)
                  |> Llm_provider.Pricing.annotate_response_cost
                  |> fun r -> patch_latency r lat)
           | Provider.Openai_chat_completions ->
               (match parse_openai_response_result body_str with
                | Ok resp ->
                    Ok (Llm_provider.Pricing.annotate_response_cost resp
                        |> fun r -> patch_latency r lat)
                | Error msg -> Error (Retry.InvalidRequest { message = msg }))
           | Provider.Custom name ->
               (match Provider.find_provider name with
                | Some impl ->
                    Ok
                      (impl.parse_response body_str
                       |> Llm_provider.Pricing.annotate_response_cost
                       |> fun r -> patch_latency r lat)
                | None ->
                    (match parse_openai_response_result body_str with
                     | Ok resp ->
                         Ok (Llm_provider.Pricing.annotate_response_cost resp
                             |> fun r -> patch_latency r lat)
                     | Error msg -> Error (Retry.InvalidRequest { message = msg }))))
      | status ->
          let code = Cohttp.Code.code_of_status status in
          let body_str = Eio.Buf_read.(of_flow ~max_size:max_response_body body |> take_all) in
          Error (Retry.classify_error ~status:code ~body:body_str)
    with
    | Eio.Io _ as exn ->
      Error (Retry.NetworkError { message = Printexc.to_string exn })
    | Unix.Unix_error _ as exn ->
      Error (Retry.NetworkError { message = Printexc.to_string exn })
    (* Backend_gemini.Gemini_api_error and Backend_glm.Glm_api_error
       are intentionally NOT caught here: this function only
       dispatches [Anthropic_messages | Openai_chat_completions |
       Custom] (see the match on [kind] above), so the Gemini/GLM
       response parsers are never invoked on this path and those
       exceptions cannot reach here. They are caught at their real
       live site in [Llm_provider.Complete] — see
       lib/llm_provider/complete.ml:271,274. *)
    | Failure msg ->
      Error (Retry.NetworkError { message = msg })
    | Yojson.Json_error msg ->
      Error (Retry.NetworkError { message = "JSON parse error: " ^ msg })
  in
  match clock with
  | Some clock ->
      (match Retry.with_retry ~clock ?config:retry_config do_request with
       | Ok _ as success -> success
       | Error err -> Error (Error.Api err))
  | None ->
      (match do_request () with
       | Ok _ as success -> success
       | Error err -> Error (Error.Api err))

[@@@coverage off]
(* === Inline tests === *)

let%test "re-exported default_base_url is non-empty" =
  String.length default_base_url > 0

let%test "re-exported api_version is non-empty" =
  String.length api_version > 0

let%test "re-exported max_response_body is positive" =
  max_response_body > 0

let%test "string_is_blank true for empty" =
  string_is_blank "" = true

let%test "string_is_blank true for spaces" =
  string_is_blank "   " = true

let%test "string_is_blank false for content" =
  string_is_blank "hello" = false

let%test "json_of_string_or_raw valid json" =
  match json_of_string_or_raw "{\"key\":\"val\"}" with
  | `Assoc [("key", `String "val")] -> true
  | _ -> false

let%test "json_of_string_or_raw invalid json returns raw assoc" =
  match json_of_string_or_raw "not json" with
  | `Assoc [("raw", `String "not json")] -> true
  | _ -> false

let%test "content_block_to_json text block" =
  let json = content_block_to_json (Types.Text "hello") in
  let open Yojson.Safe.Util in
  json |> member "type" |> to_string = "text"
  && json |> member "text" |> to_string = "hello"

let%test "content_block_of_json text block" =
  let json = `Assoc [("type", `String "text"); ("text", `String "hi")] in
  match content_block_of_json json with
  | Some (Types.Text "hi") -> true
  | _ -> false

let%test "message_to_json user message" =
  let msg : Types.message = {
    role = User; content = [Types.Text "test"];
    name = None; tool_call_id = None } in
  let json = message_to_json msg in
  let open Yojson.Safe.Util in
  json |> member "role" |> to_string = "user"

let%test "text_blocks_to_string joins text" =
  let blocks = [Types.Text "a"; Types.Text "b"] in
  let result = text_blocks_to_string blocks in
  String.length result > 0

(* --- Additional coverage tests for api.ml --- *)

let%test "string_is_blank tab only" =
  string_is_blank "\t" = true

let%test "string_is_blank newline only" =
  string_is_blank "\n" = true

let%test "string_is_blank mixed whitespace" =
  string_is_blank " \t\n " = true

let%test "string_is_blank single char" =
  string_is_blank "x" = false

let%test "json_of_string_or_raw empty string" =
  match json_of_string_or_raw "" with
  | `Assoc [("raw", `String "")] -> true
  | _ -> false

let%test "json_of_string_or_raw integer string" =
  match json_of_string_or_raw "42" with
  | `Int 42 -> true
  | _ -> false

let%test "json_of_string_or_raw array" =
  match json_of_string_or_raw "[1,2,3]" with
  | `List _ -> true
  | _ -> false

let%test "json_of_string_or_raw null" =
  match json_of_string_or_raw "null" with
  | `Null -> true
  | _ -> false

let%test "content_block_to_json tool_use block" =
  let block = Types.ToolUse { id = "t1"; name = "fn"; input = `Assoc [("x", `Int 1)] } in
  let json = content_block_to_json block in
  let open Yojson.Safe.Util in
  json |> member "type" |> to_string = "tool_use"

let%test "content_block_of_json tool_use block" =
  let json = `Assoc [
    ("type", `String "tool_use");
    ("id", `String "t1");
    ("name", `String "fn");
    ("input", `Assoc [("x", `Int 1)]);
  ] in
  match content_block_of_json json with
  | Some (Types.ToolUse { id = "t1"; name = "fn"; _ }) -> true
  | _ -> false

let%test "content_block_of_json tool_result" =
  let json = `Assoc [
    ("type", `String "tool_result");
    ("tool_use_id", `String "t1");
    ("content", `String "ok");
  ] in
  match content_block_of_json json with
  | Some (Types.ToolResult { tool_use_id = "t1"; content = "ok"; _ }) -> true
  | _ -> false

let%test "content_block_of_json unknown type" =
  let json = `Assoc [("type", `String "unknown_type")] in
  content_block_of_json json = None

let%test "message_to_json assistant message" =
  let msg : Types.message = {
    role = Assistant; content = [Types.Text "response"];
    name = None; tool_call_id = None } in
  let json = message_to_json msg in
  let open Yojson.Safe.Util in
  json |> member "role" |> to_string = "assistant"

let%test "text_blocks_to_string empty" =
  text_blocks_to_string [] = ""

let%test "text_blocks_to_string single" =
  let result = text_blocks_to_string [Types.Text "only"] in
  String.length result > 0

let%test "text_blocks_to_string non-text blocks ignored" =
  let blocks = [
    Types.ToolUse { id = "t"; name = "f"; input = `Null };
    Types.Text "visible";
  ] in
  let result = text_blocks_to_string blocks in
  String.length result > 0

(* --- patch_latency tests --- *)

let%test "patch_latency creates telemetry when None with measured ms" =
  let resp : Types.api_response = {
    id = "r1"; model = "m"; stop_reason = Types.EndTurn;
    content = []; usage = None; telemetry = None
  } in
  let patched = patch_latency resp 500 in
  match patched.telemetry with
  | Some t -> t.Llm_provider.Types.request_latency_ms = 500
  | None -> false

let%test "patch_latency overwrites existing request_latency_ms" =
  let telemetry : Llm_provider.Types.inference_telemetry = {
    system_fingerprint = Some "fp";
    timings = None;
    reasoning_tokens = Some 10;
    request_latency_ms = 0;  (* parser sentinel *)
    provider_kind = Some "anthropic";
    reasoning_effort = None;
    canonical_model_id = Some "claude-4-sonnet";
    effective_context_window = Some 200_000;
  } in
  let resp : Types.api_response = {
    id = "r2"; model = "m"; stop_reason = Types.EndTurn;
    content = []; usage = None; telemetry = Some telemetry
  } in
  let patched = patch_latency resp 1234 in
  match patched.telemetry with
  | Some t ->
    t.request_latency_ms = 1234
    && t.system_fingerprint = Some "fp"        (* preserved *)
    && t.reasoning_tokens = Some 10            (* preserved *)
    && t.canonical_model_id = Some "claude-4-sonnet" (* preserved *)
  | None -> false

let%test "patch_latency zero latency still patches" =
  let resp : Types.api_response = {
    id = "r3"; model = "m"; stop_reason = Types.EndTurn;
    content = []; usage = None; telemetry = None
  } in
  let patched = patch_latency resp 0 in
  (* Even 0 gets wrapped in Some — not a no-op. Caller decides semantics. *)
  Option.is_some patched.telemetry
