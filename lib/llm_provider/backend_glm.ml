(** ZhipuAI GLM native backend.

    OpenAI wire format with GLM-specific extensions:
    - [thinking] parameter: [{"type":"enabled","clear_thinking":true}]
    - [reasoning_content] in response message and streaming delta
    - String error codes (e.g., ["1305"])
    - Temperature range 0-1 (not 0-2)

    Ref: docs.z.ai/api-reference/llm/chat-completion

    @since 0.83.0 *)

open Types

type glm_error_class =
  | Glm_quota_exceeded
  | Glm_rate_limited
  | Glm_auth_error
  | Glm_server_error
  | Glm_invalid_request

type glm_error = {
  code: string;
  message: string;
  error_class: glm_error_class;
}

let contains_ci ~haystack ~needle =
  let h = String.lowercase_ascii haystack in
  let n = String.lowercase_ascii needle in
  let nlen = String.length n in
  let hlen = String.length h in
  if nlen = 0 || nlen > hlen then false
  else
    let rec scan i =
      if i > hlen - nlen then false
      else if String.sub h i nlen = n then true
      else scan (i + 1)
    in
    scan 0

(** Classify GLM error by code first, message fallback.
    Code mapping from docs.z.ai/api-reference/api-code:
    - 1000-1004,1100-1120: auth/account
    - 1200-1261: parameter/request (non-cascadeable)
    - 1113: account arrears (quota)
    - 1300: policy block (non-cascadeable)
    - 1301: unsafe content (non-cascadeable)
    - 1302,1303,1305,1312: transient rate/load limit (cascadeable+retryable)
    - 1304,1308,1310: quota exhausted (cascadeable, not retryable)
    - 1309,1311,1313: subscription/plan (quota)
    - 1230,1234,500: server error *)
let classify_glm_error ~code ~message : glm_error_class =
  match code with
  | "1000" | "1001" | "1002" | "1003" | "1004"
  | "1100" | "1110" | "1111" | "1112" | "1120"
  | "1220" -> Glm_auth_error
  | "1302" | "1303" | "1305" | "1312" -> Glm_rate_limited
  | "1113" | "1304" | "1308" | "1309" | "1310"
  | "1311" | "1313" -> Glm_quota_exceeded
  | "1230" | "1234" | "500" -> Glm_server_error
  | "1300" | "1301" | "1200" | "1210" | "1211"
  | "1212" | "1213" | "1214" | "1215" | "1231"
  | "1261" -> Glm_invalid_request
  | _ ->
    if contains_ci ~haystack:message ~needle:"usage limit"
       || contains_ci ~haystack:message ~needle:"quota"
       || contains_ci ~haystack:message ~needle:"exceeded" then
      Glm_quota_exceeded
    else if contains_ci ~haystack:message ~needle:"rate limit" then
      Glm_rate_limited
    else
      Glm_invalid_request

let http_code_of_glm_error_class = function
  | Glm_quota_exceeded -> 429
  | Glm_rate_limited -> 429
  | Glm_auth_error -> 401
  | Glm_server_error -> 500
  | Glm_invalid_request -> 400

exception Glm_api_error of glm_error

(* ── Request building ────────────────────────────── *)

let build_request ?(stream=false) ~(config : Provider_config.t)
    ~(messages : message list) ?(tools : Yojson.Safe.t list = []) () =
  let base_body = Backend_openai.build_request ~stream ~config ~messages ~tools () in
  match Yojson.Safe.from_string base_body with
  | `Assoc fields ->
      let fields =
        match config.enable_thinking with
        | Some true ->
            let clear_thinking =
              Option.value ~default:true config.clear_thinking
            in
            let thinking = `Assoc [
              ("type", `String "enabled");
              ("clear_thinking", `Bool clear_thinking);
            ] in
            ("thinking", thinking) :: fields
        | Some false ->
            let thinking = `Assoc [("type", `String "disabled")] in
            ("thinking", thinking) :: fields
        | None -> fields
      in
      (* Strip chat_template_kwargs leaked from Backend_openai.
         GLM does not recognize this llama.cpp/Ollama-specific field and
         returns "Invalid API parameter" when present.  GLM thinking is
         handled above via the native [thinking] parameter. *)
      let fields =
        List.filter (fun (k, _) -> k <> "chat_template_kwargs") fields
      in
      let fields =
        if stream && config.tool_stream then
          ("tool_stream", `Bool true) :: fields
        else
          fields
      in
      Yojson.Safe.to_string (`Assoc fields)
  | _ -> base_body

(* ── Response parsing ────────────────────────────── *)

(** GLM error responses use string error codes:
    [{"error":{"code":"1305","message":"..."}}]
    Standard OpenAI uses numeric HTTP codes. *)
let check_glm_error body : glm_error option =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    match json |> member "error" with
    | `Null | `Assoc [] -> None
    | err ->
        let code = match err |> member "code" with
          | `String s -> s
          | `Int n -> string_of_int n
          | _ -> "unknown"
        in
        let message = err |> member "message" |> to_string_option
          |> Option.value ~default:"Unknown GLM API error"
        in
        let error_class = classify_glm_error ~code ~message in
        Some { code; message; error_class }
  with Yojson.Json_error _ -> None

(** Extract reasoning_content from GLM response and prepend as Thinking block.
    GLM returns reasoning in [message.reasoning_content] alongside [message.content]. *)
let extract_reasoning_content (resp : api_response) body : api_response =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let choices = json |> member "choices" in
    match choices with
    | `List (choice :: _) ->
        let msg = choice |> member "message" in
        let reasoning = msg |> member "reasoning_content" |> to_string_option in
        (match reasoning with
         | Some r when String.trim r <> "" ->
             let thinking_block = Thinking { thinking_type = "thinking"; content = r } in
             { resp with content = thinking_block :: resp.content }
         | _ -> resp)
    | _ -> resp
  with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> resp

let parse_response body =
  match check_glm_error body with
  | Some err -> raise (Glm_api_error err)
  | None ->
      (match Backend_openai_parse.parse_openai_response_result body with
       | Error msg ->
           raise (Glm_api_error { code = "parse"; message = msg;
                                  error_class = Glm_invalid_request })
       | Ok resp -> extract_reasoning_content resp body)

(* ── Streaming ───────────────────────────────────── *)

(** Parse GLM SSE chunk.  GLM uses OpenAI SSE format but adds
    [delta.reasoning_content] for thinking. We parse this as
    [delta_reasoning] in the openai_chunk type. *)
let parse_stream_chunk = Streaming.parse_openai_sse_chunk

(* ── Inline tests ────────────────────────────────── *)

[@@@coverage off]

let%test "build_request without thinking is passthrough" =
  let config = Provider_config.make
    ~kind:Glm ~model_id:"glm-4.7" ~base_url:"https://open.bigmodel.cn/api/paas/v4" () in
  let messages = [{ role = User; content = [Text "hello"]; name = None; tool_call_id = None ; metadata = []}] in
  let body = build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "thinking" = `Null

let%test "build_request with thinking injects correct format" =
  let config = Provider_config.make
    ~kind:Glm ~model_id:"glm-4.5"
    ~base_url:"https://open.bigmodel.cn/api/paas/v4"
    ~enable_thinking:true () in
  let messages = [{ role = User; content = [Text "reason"]; name = None; tool_call_id = None ; metadata = []}] in
  let body = build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  thinking |> member "type" |> to_string = "enabled"
  && thinking |> member "clear_thinking" |> to_bool = true

let%test "build_request can preserve reasoning on demand" =
  let config = Provider_config.make
    ~kind:Glm ~model_id:"glm-5"
    ~base_url:"https://api.z.ai/api/coding/paas/v4"
    ~enable_thinking:true ~clear_thinking:false () in
  let messages = [{ role = User; content = [Text "reason"]; name = None; tool_call_id = None ; metadata = []}] in
  let body = build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "thinking" |> member "clear_thinking" |> to_bool = false

let%test "build_request with thinking=false injects disabled" =
  let config = Provider_config.make
    ~kind:Glm ~model_id:"glm-4.5"
    ~base_url:"https://open.bigmodel.cn/api/paas/v4"
    ~enable_thinking:false () in
  let messages = [{ role = User; content = [Text "no think"]; name = None; tool_call_id = None ; metadata = []}] in
  let body = build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  thinking |> member "type" |> to_string = "disabled"
  && json |> member "chat_template_kwargs" = `Null

let%test "check_glm_error detects string code" =
  let body = {|{"error":{"code":"1305","message":"service overloaded"}}|} in
  match check_glm_error body with
  | Some err -> err.code = "1305" && err.error_class = Glm_rate_limited
  | None -> false

let%test "check_glm_error returns None for valid response" =
  let body = {|{"choices":[{"message":{"content":"hi"}}]}|} in
  check_glm_error body = None

let%test "check_glm_error handles int code" =
  let body = {|{"error":{"code":400,"message":"bad request"}}|} in
  match check_glm_error body with
  | Some err -> err.code = "400"
  | None -> false

let%test "classify quota exceeded from message" =
  classify_glm_error ~code:"unknown" ~message:"You have reached your specified API usage limits" = Glm_quota_exceeded

let%test "classify quota from code 1113 (arrears)" =
  classify_glm_error ~code:"1113" ~message:"whatever" = Glm_quota_exceeded

let%test "classify auth from code 1001" =
  classify_glm_error ~code:"1001" ~message:"whatever" = Glm_auth_error

let%test "classify quota from code 1304 (daily limit)" =
  classify_glm_error ~code:"1304" ~message:"whatever" = Glm_quota_exceeded

let%test "classify quota from code 1308 (usage limit)" =
  classify_glm_error ~code:"1308" ~message:"whatever" = Glm_quota_exceeded

let%test "classify rate limited from code 1305" =
  classify_glm_error ~code:"1305" ~message:"whatever" = Glm_rate_limited

let%test "classify invalid request from code 1301 (unsafe content)" =
  classify_glm_error ~code:"1301" ~message:"whatever" = Glm_invalid_request

let%test "http_code quota maps to 429" =
  http_code_of_glm_error_class Glm_quota_exceeded = 429

let%test "http_code auth maps to 401" =
  http_code_of_glm_error_class Glm_auth_error = 401

let%test "extract_reasoning_content prepends thinking block" =
  let resp = { id = "x"; model = "glm-4.7"; stop_reason = EndTurn;
               content = [Text "answer"]; usage = None; telemetry = None } in
  let body = {|{"choices":[{"message":{"content":"answer","reasoning_content":"step by step"}}]}|} in
  let result = extract_reasoning_content resp body in
  match result.content with
  | Thinking { content = "step by step"; _ } :: Text "answer" :: [] -> true
  | _ -> false

let%test "extract_reasoning_content skips empty reasoning" =
  let resp = { id = "x"; model = "glm-4.7"; stop_reason = EndTurn;
               content = [Text "answer"]; usage = None; telemetry = None } in
  let body = {|{"choices":[{"message":{"content":"answer"}}]}|} in
  let result = extract_reasoning_content resp body in
  List.length result.content = 1

let%test "parse_stream_chunk delegates to openai" =
  let data = {|{"id":"x","choices":[{"delta":{"content":"hi"},"index":0}]}|} in
  match parse_stream_chunk data with
  | Some chunk -> chunk.delta_content = Some "hi"
  | None -> false

let%test "build_request strips chat_template_kwargs from GLM body" =
  let config = Provider_config.make
    ~kind:Glm ~model_id:"glm-5.1"
    ~base_url:"https://api.z.ai/api/coding/paas/v4"
    ~enable_thinking:true () in
  let messages = [{ role = User; content = [Text "hi"]; name = None; tool_call_id = None ; metadata = []}] in
  let body = build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "chat_template_kwargs" = `Null
  && json |> member "thinking" |> member "type" |> to_string = "enabled"

let%test "build_request adds tool_stream when enabled" =
  let config = Provider_config.make
    ~kind:Glm ~model_id:"glm-5.1"
    ~base_url:"https://api.z.ai/api/paas/v4"
    ~tool_stream:true () in
  let messages = [{ role = User; content = [Text "weather"]; name = None; tool_call_id = None ; metadata = []}] in
  let body = build_request ~stream:true ~config ~messages ~tools:[`Assoc [("name", `String "weather")]] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "tool_stream" |> to_bool
