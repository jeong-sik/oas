(** ZhipuAI Glm native backend.

    Openai wire format with Glm-specific extensions:
    - thinking control parameter with type=enabled and clear_thinking=true
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

type glm_error =
  { code : string
  ; message : string
  ; error_class : glm_error_class
  ; is_retryable : bool
  }

(** Classify Glm error by code first, message fallback.
    Code mapping from docs.z.ai/api-reference/api-code:
    - 1000-1004,1100-1120: auth/account
    - 1200-1261: parameter/request (terminal)
    - 1113: account arrears (quota)
    - 1300: policy block (terminal)
    - 1301: unsafe content (terminal)
    - 1302,1303,1305,1312: transient rate/load limit (retryable)
    - 1304,1308,1310: quota exhausted (not retryable)
    - 1309,1311,1313: subscription/plan (quota)
    - 1230,1234,500: server error *)
let classify_glm_error ~code ~message : glm_error_class * bool =
  match code with
  | "1000"
  | "1001"
  | "1002"
  | "1003"
  | "1004"
  | "1100"
  | "1110"
  | "1111"
  | "1112"
  | "1120"
  | "1220" -> Glm_auth_error, false
  | "1302" | "1303" | "1305" | "1312" -> Glm_rate_limited, true
  | "1113" | "1304" | "1308" | "1309" | "1310" | "1311" | "1313" ->
    Glm_quota_exceeded, false
  | "1230" | "1234" | "500" -> Glm_server_error, true
  | "1300"
  | "1301"
  | "1200"
  | "1210"
  | "1211"
  | "1212"
  | "1213"
  | "1214"
  | "1215"
  | "1231"
  | "1261" -> Glm_invalid_request, false
  | unknown_code ->
    let (_ : string) = unknown_code in
    if
      Retry.contains_case_insensitive ~haystack:message ~needle:"usage limit"
      || Retry.contains_case_insensitive ~haystack:message ~needle:"quota"
      || Retry.contains_case_insensitive ~haystack:message ~needle:"exceeded"
    then Glm_quota_exceeded, false
    else if Retry.contains_case_insensitive ~haystack:message ~needle:"rate limit"
    then Glm_rate_limited, true
    else Glm_invalid_request, false
;;

let http_code_of_glm_error_class = function
  | Glm_quota_exceeded -> 429
  | Glm_rate_limited -> 429
  | Glm_auth_error -> 401
  | Glm_server_error -> 500
  | Glm_invalid_request -> 400
;;

exception Glm_api_error of glm_error

(* ── Request building ────────────────────────────── *)

let without_field name fields =
  List.filter (fun (key, _) -> not (String.equal key name)) fields
;;

let replace_or_append_field name value fields =
  let rec loop acc replaced = function
    | [] -> if replaced then List.rev acc else List.rev ((name, value) :: acc)
    | (key, _) :: rest when String.equal key name ->
      let acc = if replaced then acc else (name, value) :: acc in
      loop acc true rest
    | field :: rest -> loop (field :: acc) replaced rest
  in
  loop [] false fields
;;

let normalize_tool_choice_fields ~(tool_choice : Types.tool_choice option) fields =
  match tool_choice with
  | Some (Auto | Any) -> replace_or_append_field "tool_choice" (`String "auto") fields
  | Some (Tool _ | None_) | None -> without_field "tool_choice" fields
;;

let build_request_with_receipt
      ?(stream = false)
      ~(config : Provider_config.t)
      ~(messages : message list)
      ?(tools : Yojson.Safe.t list = [])
      ()
  =
  (* Take the request Assoc directly from the OpenAI builder instead of
     serializing then parsing the whole message body back — removes one full
     [Yojson.Safe.to_string] (OpenAI) + one [Yojson.Safe.from_string] (here)
     per GLM turn. Byte-identical: [from_string (to_string assoc) = assoc]. *)
  let base_artifact =
    Backend_openai.build_request_assoc_with_receipt ~stream ~config ~messages ~tools ()
  in
  Provider_request_artifact.map_payload
    (function
      | `Assoc fields ->
        (* GLM thinking-control fields are emitted by the shared
       [Reasoning_dialect.request_control_fields] path inside the OpenAI
       request builder. Keep Backend_glm limited to GLM-only post-processing
       that is not a thinking builder: Z.AI's [tool_choice=auto] normalization
       and [tool_stream]. *)
        let fields =
          normalize_tool_choice_fields ~tool_choice:config.tool_choice fields
        in
        let fields =
          (* GLM streams tool-call arguments incrementally only when both
         [stream] and [tool_stream] are set; [config.tool_stream] defaults
         false, so a streaming request carrying tools would otherwise buffer
         tool args. Default [tool_stream] on when tools are present
         (RFC-OAS-023). *)
          if stream && (config.tool_stream || tools <> [])
          then ("tool_stream", `Bool true) :: fields
          else fields
        in
        Yojson.Safe.to_string (`Assoc fields)
      | (`List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null) as other
        -> Yojson.Safe.to_string other)
    base_artifact
;;

let build_request ?stream ~config ~messages ?tools () =
  build_request_with_receipt ?stream ~config ~messages ?tools ()
  |> Provider_request_artifact.payload
;;

(* ── Response parsing ────────────────────────────── *)

(** Glm error responses use string error codes:
    [{"error":{"code":"1305","message":"..."}}]
    Standard Openai uses numeric HTTP codes. *)
let check_glm_error_json (json : Yojson.Safe.t) : glm_error option =
  try
    let open Yojson.Safe.Util in
    match json |> member "error" with
    | `Null | `Assoc [] -> None
    | err ->
      let code =
        match err |> member "code" with
        | `String s -> s
        | `Int n -> string_of_int n
        | `Assoc _ | `List _ | `Intlit _ | `Float _ | `Bool _ | `Null -> "unknown"
      in
      let message =
        err
        |> member "message"
        |> to_string_option
        |> Option.value ~default:"Unknown Glm API error"
      in
      let error_class, is_retryable = classify_glm_error ~code ~message in
      Some { code; message; error_class; is_retryable }
  with
  | Yojson.Json_error _ -> None
;;

let check_glm_error body : glm_error option =
  try check_glm_error_json (Yojson.Safe.from_string body) with
  | Yojson.Json_error _ -> None
;;

(** Extract reasoning_content from Glm response and prepend as Thinking block.
    Glm returns reasoning in [message.reasoning_content] alongside [message.content]. *)
let extract_reasoning_content_json (resp : api_response) (json : Yojson.Safe.t)
  : api_response
  =
  try
    let open Yojson.Safe.Util in
    let choices = json |> member "choices" in
    match choices with
    | `List (choice :: _) ->
      let msg = choice |> member "message" in
      let reasoning = msg |> member "reasoning_content" |> to_string_option in
      (match reasoning with
       | Some r when String.trim r <> "" ->
         (match resp.content with
          | Thinking { content; _ } :: _ when String.equal content r -> resp
          | _ ->
            let thinking_block = Thinking { signature = None; content = r } in
            { resp with content = thinking_block :: resp.content })
       | Some _ | None -> resp)
    | `List [] | `Assoc _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null ->
      resp
  with
  | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> resp
;;

let extract_reasoning_content (resp : api_response) body : api_response =
  try extract_reasoning_content_json resp (Yojson.Safe.from_string body) with
  | Yojson.Json_error _ -> resp
;;

let glm_parse_error message =
  Glm_api_error
    { code = "parse"; message; error_class = Glm_invalid_request; is_retryable = false }
;;

let parse_response body =
  (* Parse the body once; a malformed body raises glm_parse_error (matching
     the prior path where check_glm_error swallowed the parse error as None
     and then parse_openai_response_result re-raised it). The three consumers
     below all traverse this same [json] instead of each re-parsing the body
     string -- was 3 full Yojson.Safe.from_string of the response per turn. *)
  let json =
    try Yojson.Safe.from_string body with
    | Yojson.Json_error msg -> raise (glm_parse_error msg)
  in
  match check_glm_error_json json with
  | Some err -> raise (Glm_api_error err)
  | None ->
    (try
       match Backend_openai_parse.parse_openai_response_result_json json with
       | Error (Backend_openai_parse.Provider_error msg) -> raise (glm_parse_error msg)
       | Error (Backend_openai_parse.Empty_completion _) ->
         (* oas#2483: GLM fails closed on an empty completion too (raises rather
            than returning an empty turn). *)
         raise
           (glm_parse_error
              "provider returned an empty assistant turn (no thinking, text, or tool \
               calls)")
       | Ok resp -> extract_reasoning_content_json resp json
     with
     | Yojson.Safe.Util.Type_error (msg, _) -> raise (glm_parse_error msg)
     | Yojson.Safe.Util.Undefined (msg, _) -> raise (glm_parse_error msg))
;;

(* ── Streaming ───────────────────────────────────── *)

(** Parse Glm SSE chunk.  Glm uses Openai SSE format but adds
    [delta.reasoning_content] for thinking. We parse this as
    [delta_reasoning] in the openai_chunk type. *)
let parse_stream_chunk data =
  Streaming.parse_openai_sse_chunk
    ~streaming_reasoning:(Reasoning_dialect.Delta_field "reasoning_content")
    data
;;

(* ── Inline tests ────────────────────────────────── *)

[@@@coverage off]

let%test "build_request without thinking is passthrough" =
  let config =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-4.7"
      ~base_url:"https://open.bigmodel.cn/api/paas/v4"
      ()
  in
  let messages =
    [ { role = User
      ; content = [ Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body = build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "thinking" = `Null
;;

let%test "build_request with thinking injects correct format" =
  let config =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-4.5"
      ~base_url:"https://open.bigmodel.cn/api/paas/v4"
      ~enable_thinking:true
      ()
  in
  let messages =
    [ { role = User
      ; content = [ Text "reason" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body = build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  thinking |> member "type" |> to_string = "enabled"
  && thinking |> member "clear_thinking" |> to_bool = true
;;

let%test "build_request can preserve reasoning on demand" =
  let config =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-5"
      ~base_url:Zai_catalog.coding_base_url
      ~enable_thinking:true
      ~clear_thinking:false
      ()
  in
  let messages =
    [ { role = User
      ; content = [ Text "reason" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body = build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "thinking" |> member "clear_thinking" |> to_bool = false
;;

let%test "build_request maps preserve_thinking to GLM clear_thinking=false" =
  let config =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-5"
      ~base_url:"https://api.z.ai/api/coding/paas/v4"
      ~enable_thinking:true
      ~preserve_thinking:true
      ()
  in
  let messages =
    [ { role = User
      ; content = [ Text "reason" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body = build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "thinking" |> member "clear_thinking" |> to_bool = false
;;

let%test
    "build_request replays reasoning via typed dialect, not serialize-time branch \
     (RFC-OAS-029 S3.1)"
  =
  (* The reasoning-replay decision now flows from the typed
     [Reasoning_dialect.replay_policy] resolved once in [for_provider_config],
     not from a serialize-time [config.kind = Glm]/[glm_should_replay_reasoning]
     branch in the request builder. A GLM config under Preserved Thinking
     (enable_thinking + clear_thinking=false) must echo a prior assistant
     [Thinking] block back as [reasoning_content]; the default config
     (clear_thinking=true) must not. Reverting the dialect resolution leaves the
     GLM dialect at the dead [No_replay] default and turns the [preserved] case
     red. *)
  let messages =
    [ { role = Assistant
      ; content =
          [ Thinking { content = "prior reasoning"; signature = None }; Text "answer" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let replays config =
    let body = build_request ~config ~messages () in
    let open Yojson.Safe.Util in
    Yojson.Safe.from_string body
    |> member "messages"
    |> to_list
    |> List.exists (fun m ->
      m |> member "reasoning_content" |> to_string_option = Some "prior reasoning")
  in
  let preserved =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-5"
      ~base_url:Zai_catalog.coding_base_url
      ~enable_thinking:true
      ~preserve_thinking:true
      ()
  in
  let default_config =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-5"
      ~base_url:Zai_catalog.coding_base_url
      ()
  in
  replays preserved && not (replays default_config)
;;

let%test "build_request preserves ZAI GLM OpenAI-compatible replay and tool content shape"
  =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"glm-5-turbo"
      ~base_url:Zai_catalog.coding_base_url
      ~enable_thinking:true
      ~preserve_thinking:true
      ()
  in
  let messages =
    [ { role = Assistant
      ; content =
          [ Thinking { content = "prior reasoning"; signature = None }
          ; ToolUse
              { id = "call_1"; name = "calc"; input = `Assoc [ "expr", `String "2+2" ] }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body = build_request ~config ~messages () |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  let assistant = body |> member "messages" |> index 0 in
  assistant |> member "content" |> to_string = ""
  && assistant |> member "reasoning_content" |> to_string = "prior reasoning"
  && assistant |> member "tool_calls" |> to_list <> []
;;

let%test "build_request emits exactly one thinking key for ZAI GLM (RFC-OAS-023)" =
  (* Regression guard for the duplicate-[thinking]-key bug: GLM thinking now
     comes from the shared OpenAI-compatible request builder. Backend_glm must
     not add a second key. Yojson [member] is blind to duplicates, so count the
     keys directly on the assoc list. *)
  let config =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-5"
      ~base_url:"https://api.z.ai/api/coding/paas/v4"
      ~enable_thinking:true
      ()
  in
  let messages =
    [ { role = User
      ; content = [ Text "reason" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body = build_request ~config ~messages () in
  match Yojson.Safe.from_string body with
  | `Assoc fields -> List.length (List.filter (fun (k, _) -> k = "thinking") fields) = 1
  | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> false
;;

let%test "build_request with thinking=false injects disabled" =
  let config =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-4.5"
      ~base_url:"https://open.bigmodel.cn/api/paas/v4"
      ~enable_thinking:false
      ()
  in
  let messages =
    [ { role = User
      ; content = [ Text "no think" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body = build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  thinking |> member "type" |> to_string = "disabled"
  && json |> member "chat_template_kwargs" = `Null
;;

let%test "check_glm_error detects string code" =
  let body = {|{"error":{"code":"1305","message":"service overloaded"}}|} in
  match check_glm_error body with
  | Some err -> err.code = "1305" && err.error_class = Glm_rate_limited
  | None -> false
;;

let%test "check_glm_error returns None for valid response" =
  let body = {|{"choices":[{"message":{"content":"hi"}}]}|} in
  check_glm_error body = None
;;

let%test "check_glm_error handles int code" =
  let body = {|{"error":{"code":400,"message":"bad request"}}|} in
  match check_glm_error body with
  | Some err -> err.code = "400"
  | None -> false
;;

let%test "classify quota exceeded from message" =
  classify_glm_error
    ~code:"unknown"
    ~message:"You have reached your specified API usage limits"
  = (Glm_quota_exceeded, false)
;;

let%test "classify quota from code 1113 (arrears)" =
  classify_glm_error ~code:"1113" ~message:"whatever" = (Glm_quota_exceeded, false)
;;

let%test "classify auth from code 1001" =
  classify_glm_error ~code:"1001" ~message:"whatever" = (Glm_auth_error, false)
;;

let%test "classify quota from code 1304 (daily limit)" =
  classify_glm_error ~code:"1304" ~message:"whatever" = (Glm_quota_exceeded, false)
;;

let%test "classify quota from code 1308 (usage limit)" =
  classify_glm_error ~code:"1308" ~message:"whatever" = (Glm_quota_exceeded, false)
;;

let%test "classify rate limited from code 1305" =
  classify_glm_error ~code:"1305" ~message:"whatever" = (Glm_rate_limited, true)
;;

let%test "classify invalid request from code 1301 (unsafe content)" =
  classify_glm_error ~code:"1301" ~message:"whatever" = (Glm_invalid_request, false)
;;

let%test "http_code quota maps to 429" =
  http_code_of_glm_error_class Glm_quota_exceeded = 429
;;

let%test "http_code auth maps to 401" = http_code_of_glm_error_class Glm_auth_error = 401

let%test "extract_reasoning_content prepends thinking block" =
  let resp =
    { id = "x"
    ; model = "glm-4.7"
    ; stop_reason = EndTurn
    ; content = [ Text "answer" ]
    ; usage = None
    ; telemetry = None
    }
  in
  let body =
    {|{"choices":[{"message":{"content":"answer","reasoning_content":"step by step"}}]}|}
  in
  let result = extract_reasoning_content resp body in
  match result.content with
  | [ Thinking { content = "step by step"; _ }; Text "answer" ] -> true
  | [] | [ _ ] | [ _; _ ] | _ :: _ :: _ -> false
;;

let%test "extract_reasoning_content skips empty reasoning" =
  let resp =
    { id = "x"
    ; model = "glm-4.7"
    ; stop_reason = EndTurn
    ; content = [ Text "answer" ]
    ; usage = None
    ; telemetry = None
    }
  in
  let body = {|{"choices":[{"message":{"content":"answer"}}]}|} in
  let result = extract_reasoning_content resp body in
  List.length result.content = 1
;;

let%test "parse_stream_chunk delegates to openai" =
  let data = {|{"id":"x","choices":[{"delta":{"content":"hi"},"index":0}]}|} in
  match parse_stream_chunk data with
  | Some chunk -> chunk.delta_content = Some "hi"
  | None -> false
;;

let%test "build_request strips chat_template_kwargs from Glm body" =
  let config =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-5.1"
      ~base_url:"https://api.z.ai/api/coding/paas/v4"
      ~enable_thinking:true
      ()
  in
  let messages =
    [ { role = User
      ; content = [ Text "hi" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body = build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "chat_template_kwargs" = `Null
  && json |> member "thinking" |> member "type" |> to_string = "enabled"
;;

let%test "build_request adds tool_stream when enabled" =
  let config =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-5.1"
      ~base_url:"https://api.z.ai/api/paas/v4"
      ~tool_stream:true
      ()
  in
  let messages =
    [ { role = User
      ; content = [ Text "weather" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body =
    build_request
      ~stream:true
      ~config
      ~messages
      ~tools:[ `Assoc [ "name", `String "weather" ] ]
      ()
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "tool_stream" |> to_bool
;;

let%test "build_request defaults tool_stream on for streaming + tools (RFC-OAS-023)" =
  (* GLM streams tool-call args incrementally only when both [stream] and
     [tool_stream] are set, but [config.tool_stream] defaults false. A
     streaming request carrying tools now defaults [tool_stream] on so the
     args arrive incrementally instead of buffered. *)
  let config =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-5.1"
      ~base_url:"https://api.z.ai/api/paas/v4"
      ()
  in
  let () = assert (not config.tool_stream) in
  let messages =
    [ { role = User
      ; content = [ Text "weather" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body =
    build_request
      ~stream:true
      ~config
      ~messages
      ~tools:[ `Assoc [ "name", `String "weather" ] ]
      ()
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "tool_stream" |> to_bool
;;
