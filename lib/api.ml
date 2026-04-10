(** API dispatch — re-exports provider modules and routes create_message *)

open Types

type named_cascade = {
  name : string;
  defaults : string list;
  config_path : string option;
  metrics : Llm_provider.Metrics.t;
  provider_filter : string list option;
}

type response_accept = Types.api_response -> (unit, string) result

let named_cascade ?config_path ?metrics ?provider_filter ~name ~defaults () =
  let metrics = match metrics with Some m -> m | None -> Llm_provider.Metrics.noop in
  { name; defaults; config_path; metrics; provider_filter }

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

let map_named_cascade_error = function
  | Llm_provider.Http_client.HttpError { code; body } ->
      Error.Api (Retry.classify_error ~status:code ~body)
  | Llm_provider.Http_client.NetworkError { message } ->
      Error.Api (Retry.NetworkError { message })

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
    try
      let resp, body = Cohttp_eio.Client.post ~sw client ~headers ~body:(Cohttp_eio.Body.of_string body_str) uri in
      match Cohttp.Response.status resp with
      | `OK ->
          let body_str = Eio.Buf_read.(of_flow ~max_size:max_response_body body |> take_all) in
          (match kind with
           | Provider.Anthropic_messages ->
               Ok
                 (parse_response (Yojson.Safe.from_string body_str)
                  |> Llm_provider.Pricing.annotate_response_cost)
           | Provider.Openai_chat_completions ->
               (match parse_openai_response_result body_str with
                | Ok resp ->
                    Ok (Llm_provider.Pricing.annotate_response_cost resp)
                | Error msg -> Error (Retry.InvalidRequest { message = msg }))
           | Provider.Custom name ->
               (match Provider.find_provider name with
                | Some impl ->
                    Ok
                      (impl.parse_response body_str
                       |> Llm_provider.Pricing.annotate_response_cost)
                | None ->
                    (match parse_openai_response_result body_str with
                     | Ok resp ->
                         Ok (Llm_provider.Pricing.annotate_response_cost resp)
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
    | Llm_provider.Backend_gemini.Gemini_api_error msg ->
      Error (Retry.InvalidRequest { message = msg })
    | Llm_provider.Backend_glm.Glm_api_error msg ->
      Error (Retry.InvalidRequest { message = msg })
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

(** Send a message with cascade failover across providers.
    Tries primary provider first (with retries); on any failure,
    tries each fallback provider in order. *)
let create_message_cascade ~sw ~net ?clock ?retry_config
    ~cascade:(casc : Provider.cascade) ~config ~messages ?tools () =
  let make_f provider_cfg () =
    match create_message ~sw ~net ~provider:provider_cfg ?clock
            ~config ~messages ?tools () with
    | Ok r -> Ok r
    | Error (Error.Api err) -> Error err
    | Error _other ->
      Error (Retry.NetworkError { message = "Non-API error during cascade" })
  in
  match clock with
  | Some clock ->
    let retry_cfg = match retry_config with
      | Some c -> c | None -> Retry.default_config in
    let primary = make_f casc.primary in
    let fallbacks = List.map make_f casc.fallbacks in
    (match Retry.with_cascade ~clock ~config:retry_cfg ~primary ~fallbacks () with
     | Ok _ as success -> success
     | Error err -> Error (Error.Api err))
  | None ->
    (* Without a clock, retries are disabled but we still try fallbacks
       sequentially on any error.  A non-retryable error on provider A
       (e.g. AuthError) should still attempt provider B — the key may be
       valid there.  Matches Retry.with_cascade semantics (PR #336). *)
    let rec try_providers primary_err = function
      | [] -> Error (Error.Api primary_err)
      | provider :: rest ->
          match make_f provider () with
          | Ok _ as success -> success
          | Error _err -> try_providers primary_err rest
    in
    (match make_f casc.primary () with
     | Ok _ as success -> success
     | Error err -> try_providers err casc.fallbacks)

let create_message_named ~sw ~net ?clock ~(named_cascade : named_cascade)
    ~config ~messages ?tools ?(temperature = 0.3)
    ?(max_tokens = config.config.max_tokens)
    ?system_prompt ?(accept = fun _ -> true) ?accept_reason
    ?(accept_on_exhaustion = false)
    ?timeout_sec ?metrics ?priority () =
  let system_prompt =
    match system_prompt with
    | Some _ -> system_prompt
    | None -> config.config.system_prompt
  in
  (* Fall back to named_cascade.metrics when caller does not provide ?metrics.
     This lets downstream consumers wire Prometheus counters once via
     named_cascade and get cache hit/miss callbacks without threading
     ?metrics through every call site. *)
  let metrics = match metrics with
    | Some m -> m
    | None -> named_cascade.metrics
  in
  let accept_result =
    Completion_contract.resolve_accept ~accept ?accept_reason
  in
  match
    Llm_provider.Cascade_config.complete_named ~sw ~net ?clock
      ?config_path:named_cascade.config_path ~name:named_cascade.name
      ~defaults:named_cascade.defaults ~messages ?tools ~temperature
      ~max_tokens ?system_prompt ?tool_choice:config.config.tool_choice
      ~accept_reason:accept_result ~accept_on_exhaustion ?timeout_sec ~metrics ?priority
      ?provider_filter:named_cascade.provider_filter ()
  with
  | Ok response -> Ok response
  | Error err -> Error (map_named_cascade_error err)

let create_message_named_stream ~sw ~net ?clock
    ~(named_cascade : named_cascade) ~config ~messages ?tools
    ?(temperature = 0.3) ?(max_tokens = config.config.max_tokens)
    ?system_prompt ?timeout_sec ?metrics ~on_event ?priority () =
  let system_prompt =
    match system_prompt with
    | Some _ -> system_prompt
    | None -> config.config.system_prompt
  in
  let metrics = match metrics with
    | Some m -> m
    | None -> named_cascade.metrics
  in
  match
    Llm_provider.Cascade_config.complete_named_stream ~sw ~net ?clock
      ?config_path:named_cascade.config_path ~name:named_cascade.name
      ~defaults:named_cascade.defaults ~messages ?tools ~temperature
      ~max_tokens ?system_prompt ?tool_choice:config.config.tool_choice
      ?timeout_sec ~metrics ~on_event ?priority
      ?provider_filter:named_cascade.provider_filter ()
  with
  | Ok response -> Ok response
  | Error err -> Error (map_named_cascade_error err)

[@@@coverage off]
(* === Inline tests === *)

let%test "named_cascade creates record" =
  let nc = named_cascade ~name:"test" ~defaults:["llama:m1"] () in
  nc.name = "test" && nc.defaults = ["llama:m1"] && nc.config_path = None

let%test "named_cascade with config_path" =
  let nc = named_cascade ~config_path:"/some/path.json" ~name:"n" ~defaults:[] () in
  nc.config_path = Some "/some/path.json"

let%test "map_named_cascade_error HttpError" =
  let err = Llm_provider.Http_client.HttpError { code = 429; body = "rate limited" } in
  match map_named_cascade_error err with
  | Error.Api _ -> true
  | _ -> false

let%test "map_named_cascade_error NetworkError" =
  let err = Llm_provider.Http_client.NetworkError { message = "timeout" } in
  match map_named_cascade_error err with
  | Error.Api (Retry.NetworkError { message }) -> message = "timeout"
  | _ -> false

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

let%test "map_named_cascade_error HttpError 500" =
  let err = Llm_provider.Http_client.HttpError { code = 500; body = "server error" } in
  match map_named_cascade_error err with
  | Error.Api _ -> true
  | _ -> false

let%test "map_named_cascade_error HttpError 429" =
  let err = Llm_provider.Http_client.HttpError { code = 429; body = "rate limit" } in
  match map_named_cascade_error err with
  | Error.Api _ -> true
  | _ -> false

let%test "named_cascade defaults" =
  let nc = named_cascade ~name:"default" ~defaults:[] () in
  nc.name = "default" && nc.defaults = [] && nc.config_path = None

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
