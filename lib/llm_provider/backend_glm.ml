(** ZhipuAI GLM native backend.

    OpenAI wire format with GLM-specific extensions:
    - [thinking] parameter: [{"type":"enabled","clear_thinking":true}]
    - [reasoning_content] in response message and streaming delta
    - String error codes (e.g., ["1305"])
    - Temperature range 0-1 (not 0-2)

    Ref: docs.z.ai/api-reference/llm/chat-completion

    @since 0.83.0 *)

open Types

exception Glm_api_error of string

(* ── Request building ────────────────────────────── *)

let build_request ?(stream=false) ~(config : Provider_config.t)
    ~(messages : message list) ?(tools : Yojson.Safe.t list = []) () =
  let base_body = Backend_openai.build_request ~stream ~config ~messages ~tools () in
  match config.enable_thinking with
  | Some true ->
      (* GLM thinking uses a top-level "thinking" parameter:
         {"thinking": {"type": "enabled", "clear_thinking": true}}
         clear_thinking=true removes prior reasoning from context (saves tokens). *)
      (match Yojson.Safe.from_string base_body with
       | `Assoc fields ->
           let thinking = `Assoc [
             ("type", `String "enabled");
             ("clear_thinking", `Bool true);
           ] in
           let fields = ("thinking", thinking) :: fields in
           Yojson.Safe.to_string (`Assoc fields)
       | _ -> base_body)
  | Some false ->
      (match Yojson.Safe.from_string base_body with
       | `Assoc fields ->
           let thinking = `Assoc [("type", `String "disabled")] in
           let fields = ("thinking", thinking) :: fields in
           Yojson.Safe.to_string (`Assoc fields)
       | _ -> base_body)
  | None -> base_body

(* ── Response parsing ────────────────────────────── *)

(** GLM error responses use string error codes:
    [{"error":{"code":"1305","message":"..."}}]
    Standard OpenAI uses numeric HTTP codes. *)
let check_glm_error body =
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
        Some (Printf.sprintf "GLM error %s: %s" code message)
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
  | Some msg -> raise (Glm_api_error msg)
  | None ->
      (match Backend_openai_parse.parse_openai_response_result body with
       | Error msg -> raise (Glm_api_error msg)
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
  let messages = [{ role = User; content = [Text "hello"]; name = None; tool_call_id = None }] in
  let body = build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "thinking" = `Null

let%test "build_request with thinking injects correct format" =
  let config = Provider_config.make
    ~kind:Glm ~model_id:"glm-4.5"
    ~base_url:"https://open.bigmodel.cn/api/paas/v4"
    ~enable_thinking:true () in
  let messages = [{ role = User; content = [Text "reason"]; name = None; tool_call_id = None }] in
  let body = build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  thinking |> member "type" |> to_string = "enabled"
  && thinking |> member "clear_thinking" |> to_bool = true

let%test "build_request with thinking=false injects disabled" =
  let config = Provider_config.make
    ~kind:Glm ~model_id:"glm-4.5"
    ~base_url:"https://open.bigmodel.cn/api/paas/v4"
    ~enable_thinking:false () in
  let messages = [{ role = User; content = [Text "no think"]; name = None; tool_call_id = None }] in
  let body = build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  thinking |> member "type" |> to_string = "disabled"

let%test "check_glm_error detects string code" =
  let body = {|{"error":{"code":"1305","message":"service overloaded"}}|} in
  match check_glm_error body with
  | Some msg -> String.length msg > 0
  | None -> false

let%test "check_glm_error returns None for valid response" =
  let body = {|{"choices":[{"message":{"content":"hi"}}]}|} in
  check_glm_error body = None

let%test "check_glm_error handles int code" =
  let body = {|{"error":{"code":400,"message":"bad request"}}|} in
  match check_glm_error body with
  | Some msg -> String.length msg > 0
  | None -> false

let%test "extract_reasoning_content prepends thinking block" =
  let resp = { id = "x"; model = "glm-4.7"; stop_reason = EndTurn;
               content = [Text "answer"]; usage = None } in
  let body = {|{"choices":[{"message":{"content":"answer","reasoning_content":"step by step"}}]}|} in
  let result = extract_reasoning_content resp body in
  match result.content with
  | Thinking { content = "step by step"; _ } :: Text "answer" :: [] -> true
  | _ -> false

let%test "extract_reasoning_content skips empty reasoning" =
  let resp = { id = "x"; model = "glm-4.7"; stop_reason = EndTurn;
               content = [Text "answer"]; usage = None } in
  let body = {|{"choices":[{"message":{"content":"answer"}}]}|} in
  let result = extract_reasoning_content resp body in
  List.length result.content = 1

let%test "parse_stream_chunk delegates to openai" =
  let data = {|{"id":"x","choices":[{"delta":{"content":"hi"},"index":0}]}|} in
  match parse_stream_chunk data with
  | Some chunk -> chunk.delta_content = Some "hi"
  | None -> false
