(** OpenAI-compatible API response parsing, message serialization,
    and request building.

    Pure functions operating on {!Llm_provider.Types}.
    {!build_request} uses {!Provider_config.t} (no agent_sdk coupling).

    @since 0.92.0 decomposed into Backend_openai_serialize,
    Backend_openai_parse *)

open Types

(* ── Re-exports from serialization ─────────────────────── *)

let tool_calls_to_openai_json = Backend_openai_serialize.tool_calls_to_openai_json
let openai_content_parts_of_blocks = Backend_openai_serialize.openai_content_parts_of_blocks
let openai_messages_of_message = Backend_openai_serialize.openai_messages_of_message
let tool_choice_to_openai_json = Backend_openai_serialize.tool_choice_to_openai_json
let build_openai_tool_json = Backend_openai_serialize.build_openai_tool_json

(* ── Re-exports from parsing ──────────────────────────── *)

let strip_json_markdown_fences = Backend_openai_parse.strip_json_markdown_fences
let usage_of_openai_json = Backend_openai_parse.usage_of_openai_json

let parse_openai_response_result = Backend_openai_parse.parse_openai_response_result

(* ── Capability-drop WARN dedup ────────────────────────── *)

(** One-shot stderr WARN table, keyed by ([model_id], [field_name]).
    Reached from the capability-gated drop branches in {!build_request}
    so operators see exactly which sampling field their config was
    trying to send for which model, without the per-request WARN spam
    that would otherwise fire on every keeper turn. Double-warning on
    a race is harmless and only happens once per key. *)
let capability_drop_warned : (string * string, unit) Hashtbl.t =
  Hashtbl.create 16

let warn_capability_drop ~model_id ~field =
  let key = (model_id, field) in
  if not (Hashtbl.mem capability_drop_warned key) then begin
    Hashtbl.replace capability_drop_warned key ();
    Printf.eprintf
      "[WARN] [backend_openai] dropping sampling field %s for model %s: \
       capability record reports supports_%s = false. Update \
       Capabilities.for_model_id if this model actually supports it, \
       otherwise remove the field from your agent/cascade config.\n%!"
      field model_id field
  end

(* ── Request building ──────────────────────────────────── *)

let effective_tool_choice (config : Provider_config.t) =
  match config.kind, config.tool_choice with
  | Provider_config.Glm, Some None_ -> None
  | _, Some choice -> Some (tool_choice_to_openai_json choice)
  | _, None -> None

let effective_tools (config : Provider_config.t) tools =
  match config.kind, config.tool_choice with
  | Provider_config.Glm, Some None_ -> []
  | _ -> tools

(** Build OpenAI Chat Completions request body from {!Provider_config.t}.
    Returns a JSON string ready for HTTP POST. *)
let build_request ?(stream=false) ~(config : Provider_config.t)
    ~(messages : message list) ?(tools : Yojson.Safe.t list = []) () =
  let tools = effective_tools config tools in
  let provider_messages =
    (match config.system_prompt with
     | Some s when not (Api_common.string_is_blank s) ->
         [`Assoc [("role", `String "system"); ("content", `String (Utf8_sanitize.sanitize s))]]
     | _ -> [])
    @ List.concat_map openai_messages_of_message messages
  in
  (* Clamp max_tokens to the capability record's upper bound before
     sending. Consumers (e.g. masc-mcp cascade.json) may configure
     max_tokens well above the provider's hard limit; honouring that
     value causes a server-side 400 "max_tokens must be less than or
     equal to ..." which then fails the whole turn. The capability
     upper bound is authoritative. *)
  let caps_preview =
    match Capabilities.for_model_id config.model_id with
    | Some c -> c
    | None -> Capabilities.default_capabilities
  in
  let effective_max_tokens =
    match caps_preview.max_output_tokens with
    | Some cap when config.max_tokens > cap -> cap
    | _ -> config.max_tokens
  in
  let body =
    [ ("model", `String config.model_id);
      ("messages", `List provider_messages);
      ("max_tokens", `Int effective_max_tokens) ]
  in
  let body = match config.temperature with
    | Some t -> ("temperature", `Float t) :: body
    | None -> body
  in
  let body = match config.top_p with
    | Some p -> ("top_p", `Float p) :: body
    | None -> body
  in
  (* Look up per-model capabilities once — the sampling params below
     rely on [supports_top_k] / [supports_min_p] to decide whether the
     server will accept the field.  If no capability record exists for
     the model, default to NOT sending the non-standard sampling
     params; conservative because misclassified providers (GLM, Gemini,
     ...) previously silently inherited [config.min_p]/[config.top_k]
     from the agent config and triggered hard 400s at the server. *)
  let caps =
    match Capabilities.for_model_id config.model_id with
    | Some c -> c
    | None -> Capabilities.default_capabilities
  in
  (* Silent drops of user-supplied sampling params are a debugging
     hazard (GLM review on #830), so emit a ONE-SHOT stderr WARN per
     (model_id, field) combination the first time a drop fires. Per-
     request WARN would spam — keepers do ~50 requests/minute — hence
     the dedup table. The cell is best-effort: Eio cooperative
     scheduling means two fibers racing [mem_opt]/[replace] can
     double-warn at most once per key, which is harmless. *)
  let body = match config.top_k with
    | Some k when caps.supports_top_k -> ("top_k", `Int k) :: body
    | Some _ ->
      warn_capability_drop ~model_id:config.model_id ~field:"top_k";
      body
    | None -> body
  in
  let body = match config.min_p with
    | Some p when caps.supports_min_p -> ("min_p", `Float p) :: body
    | Some _ ->
      warn_capability_drop ~model_id:config.model_id ~field:"min_p";
      body
    | None -> body
  in
  let body = match config.enable_thinking with
    | Some enabled ->
        ("chat_template_kwargs",
         `Assoc [("enable_thinking", `Bool enabled)]) :: body
    | None -> body
  in
  (* tool_choice uses a DIFFERENT unknown-model default than top_k /
     min_p above: unknown → assume supported (true). Two reasons:
       (1) [tool_choice] is a standard OpenAI Chat Completions body
           param and virtually every OpenAI-compat server accepts it,
           so conservatively dropping it on unknown models would
           regress every agent that uses a model Capabilities does
           not know about yet.
       (2) top_k / min_p are non-standard extensions — ZAI GLM hard
           400s on them (#827/#830), so conservative drop is the
           right default for those specifically.
     That is why this lookup is NOT a dedup candidate against the
     [caps] binding above: we need [true] on [None] here, whereas
     [caps] gives [default_capabilities.supports_tool_choice = false]
     on [None]. Both defaults are intentional and contextual, not
     drift. *)
  let supports_tool_choice =
    match Capabilities.for_model_id config.model_id with
    | Some c -> c.supports_tool_choice
    | None -> true
  in
  let body = match effective_tool_choice config with
    | Some choice_json when supports_tool_choice ->
        ("tool_choice", choice_json) :: body
    | None -> body
    | Some _ -> body
  in
  let body = match tools with
    | [] -> body
    | ts ->
        ("tools", `List (List.map build_openai_tool_json ts)) :: body
  in
  let body =
    if config.disable_parallel_tool_use && tools <> [] then
      ("parallel_tool_calls", `Bool false) :: body
    else body
  in
  let body =
    if config.response_format_json then
      ("response_format",
       `Assoc [("type", `String "json_object")]) :: body
    else body
  in
  let body =
    if stream then ("stream", `Bool true) :: body
    else body
  in
  Yojson.Safe.to_string (`Assoc body)

[@@@coverage off]
(* === Inline tests === *)

let%test "tool_choice_to_openai_json Auto" =
  tool_choice_to_openai_json Auto = `String "auto"

let%test "tool_choice_to_openai_json Any" =
  tool_choice_to_openai_json Any = `String "required"

let%test "tool_choice_to_openai_json None_" =
  tool_choice_to_openai_json None_ = `String "none"

let%test "tool_choice_to_openai_json Tool name" =
  let result = tool_choice_to_openai_json (Tool "my_tool") in
  let open Yojson.Safe.Util in
  result |> member "type" |> to_string = "function"
  && result |> member "function" |> member "name" |> to_string = "my_tool"

let%test "glm preserves named tool_choice" =
  let cfg = Provider_config.make
    ~kind:Provider_config.Glm ~model_id:"glm-5"
    ~base_url:Zai_catalog.general_base_url
    ~tool_choice:(Tool "calculator") () in
  match effective_tool_choice cfg with
  | Some (`Assoc _ as json) ->
    let open Yojson.Safe.Util in
    json |> member "type" |> to_string = "function"
    && json |> member "function" |> member "name" |> to_string = "calculator"
  | _ -> false

let%test "glm preserves tool_choice any as required" =
  let cfg = Provider_config.make
    ~kind:Provider_config.Glm ~model_id:"glm-5"
    ~base_url:Zai_catalog.general_base_url
    ~tool_choice:Any () in
  match effective_tool_choice cfg with
  | Some (`String "required") -> true
  | _ -> false

let%test "glm drops tool_choice none" =
  let cfg = Provider_config.make
    ~kind:Provider_config.Glm ~model_id:"glm-5"
    ~base_url:Zai_catalog.general_base_url
    ~tool_choice:None_ () in
  effective_tool_choice cfg = None

let%test "glm drops tools when tool_choice none" =
  let cfg = Provider_config.make
    ~kind:Provider_config.Glm ~model_id:"glm-5"
    ~base_url:Zai_catalog.general_base_url
    ~tool_choice:None_ () in
  let tool_json =
    `Assoc [
      ("name", `String "calculator");
      ("description", `String "math");
      ("input_schema", `Assoc [("type", `String "object")]);
    ]
  in
  let json = build_request ~config:cfg ~messages:[] ~tools:[tool_json] ()
             |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  let assoc = to_assoc json in
  not (List.mem_assoc "tool_choice" assoc)
  && not (List.mem_assoc "tools" assoc)

(* === Capability-gated sampling param tests (oas#827) === *)

let%test "glm drops min_p when model does not support it" =
  (* GLM's glm_capabilities inherits supports_min_p = false from
     default_capabilities.  Even when the caller sets min_p explicitly
     (via cascade inheritance or agent default), backend_openai must
     omit it from the wire body — ZAI rejects the request with
     "property 'min_p' is unsupported". *)
  let cfg = Provider_config.make
    ~kind:Provider_config.Glm ~model_id:"glm-5.1"
    ~base_url:Zai_catalog.general_base_url
    ~min_p:0.05 () in
  let json = build_request ~config:cfg ~messages:[] ()
             |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  not (List.mem_assoc "min_p" (to_assoc json))

let%test "glm drops top_k when model does not support it" =
  let cfg = Provider_config.make
    ~kind:Provider_config.Glm ~model_id:"glm-5.1"
    ~base_url:Zai_catalog.general_base_url
    ~top_k:40 () in
  let json = build_request ~config:cfg ~messages:[] ()
             |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  not (List.mem_assoc "top_k" (to_assoc json))

let%test "ollama preserves min_p (llama.cpp supports it)" =
  (* qwen3 via Ollama has supports_min_p = true in qwen_capabilities.
     The capability-gated path must still pass min_p through for
     providers that do support it. *)
  let cfg = Provider_config.make
    ~kind:Provider_config.Ollama ~model_id:"qwen3.5:35b-a3b-nvfp4"
    ~base_url:"http://127.0.0.1:11434"
    ~min_p:0.05 () in
  let json = build_request ~config:cfg ~messages:[] ()
             |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  match json |> member "min_p" with
  | `Float f -> Float.abs (f -. 0.05) < 1e-6
  | _ -> false

let%test "strip_json_markdown_fences plain text unchanged" =
  strip_json_markdown_fences "{\"key\":\"value\"}" = "{\"key\":\"value\"}"

let%test "strip_json_markdown_fences strips json fences" =
  let input = "```json\n{\"key\":\"value\"}\n```" in
  strip_json_markdown_fences input = "{\"key\":\"value\"}"

let%test "strip_json_markdown_fences strips plain fences" =
  let input = "```\n{\"key\":\"value\"}\n```" in
  strip_json_markdown_fences input = "{\"key\":\"value\"}"

let%test "strip_json_markdown_fences short string unchanged" =
  strip_json_markdown_fences "hi" = "hi"

let%test "tool_calls_to_openai_json extracts ToolUse blocks" =
  let blocks = [
    Text "hello";
    ToolUse { id = "tc1"; name = "fn1"; input = `Assoc [("x", `Int 1)] };
  ] in
  let result = tool_calls_to_openai_json blocks in
  List.length result = 1

let%test "tool_calls_to_openai_json empty for no tool_use" =
  tool_calls_to_openai_json [Text "no tools"] = []

let%test "openai_content_parts_of_blocks filters text and image" =
  let blocks = [
    Text "hello";
    Thinking { thinking_type = "reasoning"; content = "..." };
    ToolUse { id = "tc1"; name = "fn"; input = `Null };
  ] in
  let result = openai_content_parts_of_blocks blocks in
  List.length result = 1

let%test "build_openai_tool_json converts input_schema to parameters" =
  let tool_json = `Assoc [
    ("name", `String "my_fn");
    ("description", `String "does stuff");
    ("input_schema", `Assoc [("type", `String "object")]);
  ] in
  let result = build_openai_tool_json tool_json in
  let open Yojson.Safe.Util in
  result |> member "type" |> to_string = "function"
  && result |> member "function" |> member "name" |> to_string = "my_fn"
  && result |> member "function" |> member "parameters" |> member "type" |> to_string = "object"

let%test "build_openai_tool_json non-assoc passthrough" =
  build_openai_tool_json (`String "bad") = `String "bad"

let%test "usage_of_openai_json parses usage" =
  let json = `Assoc [
    ("usage", `Assoc [
      ("prompt_tokens", `Int 100);
      ("completion_tokens", `Int 50);
    ]);
  ] in
  match usage_of_openai_json json with
  | Some u -> u.input_tokens = 100 && u.output_tokens = 50
  | None -> false

let%test "usage_of_openai_json null usage returns None" =
  let json = `Assoc [("usage", `Null)] in
  usage_of_openai_json json = None

let%test "usage_of_openai_json missing usage returns None" =
  let json = `Assoc [] in
  usage_of_openai_json json = None

let%test "usage_of_openai_json with cached_tokens" =
  let json = `Assoc [
    ("usage", `Assoc [
      ("prompt_tokens", `Int 100);
      ("completion_tokens", `Int 50);
      ("prompt_tokens_details", `Assoc [("cached_tokens", `Int 30)]);
    ]);
  ] in
  match usage_of_openai_json json with
  | Some u -> u.cache_read_input_tokens = 30
  | None -> false

let%test "parse_openai_response_result basic text response" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "chatcmpl-1");
    ("model", `String "gpt-4");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "stop");
        ("message", `Assoc [
          ("content", `String "Hello world");
        ]);
      ];
    ]);
  ]) in
  match parse_openai_response_result json_str with
  | Ok resp ->
    resp.id = "chatcmpl-1"
    && resp.model = "gpt-4"
    && resp.stop_reason = EndTurn
  | Error _ -> false

let%test "parse_openai_response_result tool calls" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "cmpl-2");
    ("model", `String "gpt-4");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "tool_calls");
        ("message", `Assoc [
          ("content", `Null);
          ("tool_calls", `List [
            `Assoc [
              ("id", `String "call_1");
              ("type", `String "function");
              ("function", `Assoc [
                ("name", `String "get_weather");
                ("arguments", `String "{\"city\":\"Seoul\"}");
              ]);
            ];
          ]);
        ]);
      ];
    ]);
  ]) in
  match parse_openai_response_result json_str with
  | Ok resp -> resp.stop_reason = StopToolUse
  | Error _ -> false

let%test "parse_openai_response_result max_tokens stop reason" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "cmpl-3");
    ("model", `String "m");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "length");
        ("message", `Assoc [("content", `String "truncated")]);
      ];
    ]);
  ]) in
  match parse_openai_response_result json_str with
  | Ok resp -> resp.stop_reason = MaxTokens
  | Error _ -> false

let%test "parse_openai_response_result error returns Error" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("error", `Assoc [("message", `String "rate limited")]);
  ]) in
  match parse_openai_response_result json_str with
  | Error msg -> msg = "rate limited"
  | Ok _ -> false

let%test "openai_messages_of_message user text" =
  let msg = { role = User; content = [Text "hello"]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  List.length result = 1

let%test "openai_messages_of_message user with tool_result" =
  let msg = { role = User; content = [
    Text "follow up";
    ToolResult { tool_use_id = "tc1"; content = "result"; is_error = false; json = None };
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  List.length result = 2

let%test "openai_messages_of_message assistant with tool_calls" =
  let msg = { role = Assistant; content = [
    ToolUse { id = "tc1"; name = "fn"; input = `Assoc [] };
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  List.length result = 1

let%test "openai_messages_of_message system" =
  let msg = { role = System; content = [Text "system prompt"]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  List.length result = 1

let%test "openai_messages_of_message user empty content" =
  let msg = { role = User; content = []; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  result = []

let%test "openai_messages_of_message user with image" =
  let msg = { role = User; content = [
    Image { media_type = "image/png"; data = "abc123"; source_type = "base64" };
    Text "describe this";
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  List.length result = 1

let%test "openai_messages_of_message user with document" =
  let msg = { role = User; content = [
    Document { media_type = "application/pdf"; data = "abc"; source_type = "base64" };
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  List.length result = 1

let%test "openai_messages_of_message user with audio" =
  let msg = { role = User; content = [
    Audio { media_type = "audio/wav"; data = "audiodata"; source_type = "base64" };
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  List.length result = 1

let%test "openai_messages_of_message assistant text only" =
  let msg = { role = Assistant; content = [Text "hello"]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  let json = List.hd result in
  let open Yojson.Safe.Util in
  json |> member "content" |> to_string = "hello"

let%test "openai_messages_of_message assistant blank text with tool_calls" =
  let msg = { role = Assistant; content = [
    Text "";
    ToolUse { id = "tc1"; name = "fn"; input = `Assoc [] };
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  let json = List.hd result in
  let open Yojson.Safe.Util in
  json |> member "content" = `Null

let%test "openai_messages_of_message Tool role with ToolResult" =
  let msg = { role = Tool; content = [
    ToolResult { tool_use_id = "tc1"; content = "result data"; is_error = false; json = None };
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  List.length result = 1
  && (let json = List.hd result in
      let open Yojson.Safe.Util in
      json |> member "role" |> to_string = "tool")

let%test "openai_messages_of_message Tool role without ToolResult fallback to user" =
  let msg = { role = Tool; content = [Text "fallback"]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  let json = List.hd result in
  let open Yojson.Safe.Util in
  json |> member "role" |> to_string = "user"

let%test "build_openai_tool_json with parameters field" =
  let tool_json = `Assoc [
    ("name", `String "my_fn");
    ("description", `String "does stuff");
    ("parameters", `Assoc [("type", `String "object")]);
  ] in
  let result = build_openai_tool_json tool_json in
  let open Yojson.Safe.Util in
  result |> member "function" |> member "parameters" |> member "type" |> to_string = "object"

let%test "build_openai_tool_json converts legacy parameter list to json schema" =
  let tool_json = `Assoc [
    ("name", `String "my_fn");
    ("description", `String "does stuff");
    ("parameters", `List [
      `Assoc [
        ("name", `String "query");
        ("description", `String "search query");
        ("param_type", `String "string");
        ("required", `Bool true);
      ];
      `Assoc [
        ("name", `String "limit");
        ("description", `String "max results");
        ("param_type", `String "integer");
        ("required", `Bool false);
      ];
    ]);
  ] in
  let result = build_openai_tool_json tool_json in
  let open Yojson.Safe.Util in
  let parameters = result |> member "function" |> member "parameters" in
  parameters |> member "type" |> to_string = "object"
  && parameters |> member "properties" |> member "query" |> member "type" |> to_string = "string"
  && parameters |> member "properties" |> member "limit" |> member "type" |> to_string = "integer"
  && List.mem "query" (parameters |> member "required" |> to_list |> List.map to_string)

let%test "build_openai_tool_json skips malformed legacy parameter entries" =
  let tool_json = `Assoc [
    ("name", `String "my_fn");
    ("parameters", `List [
      `Assoc [
        ("description", `String "missing name");
        ("param_type", `String "string");
        ("required", `Bool true);
      ];
      `Assoc [
        ("name", `String "query");
        ("description", `String "search query");
        ("required", `Bool true);
      ];
    ]);
  ] in
  let result = build_openai_tool_json tool_json in
  let open Yojson.Safe.Util in
  let parameters = result |> member "function" |> member "parameters" in
  parameters |> member "properties" |> member "query" |> member "type" |> to_string = "string"
  && (parameters |> member "properties" |> member "" = `Null)
  && List.mem "query" (parameters |> member "required" |> to_list |> List.map to_string)

let%test "build_openai_tool_json missing all optional fields" =
  let tool_json = `Assoc [] in
  let result = build_openai_tool_json tool_json in
  let open Yojson.Safe.Util in
  result |> member "function" |> member "name" |> to_string = "tool"
  && result |> member "function" |> member "description" |> to_string = ""

let%test "build_openai_tool_json list passthrough" =
  build_openai_tool_json (`List [`String "bad"]) = `List [`String "bad"]

let%test "strip_json_markdown_fences no closing fence" =
  let input = "```json\n{\"key\":\"value\"}" in
  strip_json_markdown_fences input = input

let%test "strip_json_markdown_fences empty content" =
  strip_json_markdown_fences "" = ""

let%test "parse_openai_response_result unknown finish_reason" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "c1");
    ("model", `String "m");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "something_new");
        ("message", `Assoc [("content", `String "text")]);
      ];
    ]);
  ]) in
  match parse_openai_response_result json_str with
  | Ok resp -> resp.stop_reason = Unknown "something_new"
  | Error _ -> false

let%test "parse_openai_response_result end_turn finish_reason" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "c1");
    ("model", `String "m");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "end_turn");
        ("message", `Assoc [("content", `String "done")]);
      ];
    ]);
  ]) in
  match parse_openai_response_result json_str with
  | Ok resp -> resp.stop_reason = EndTurn
  | Error _ -> false

let%test "parse_openai_response_result null content" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "c1");
    ("model", `String "m");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "stop");
        ("message", `Assoc [("content", `Null)]);
      ];
    ]);
  ]) in
  match parse_openai_response_result json_str with
  | Ok resp -> resp.stop_reason = EndTurn
  | Error _ -> false

let%test "parse_openai_response_result list content" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "c1");
    ("model", `String "m");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "stop");
        ("message", `Assoc [
          ("content", `List [`String "part1"; `String "part2"])
        ]);
      ];
    ]);
  ]) in
  match parse_openai_response_result json_str with
  | Ok resp ->
    (match resp.content with
     | [Text t] -> String.length t > 0
     | _ -> false)
  | Error _ -> false

let%test "parse_openai_response_result list content with assoc text blocks" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "c1");
    ("model", `String "m");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "stop");
        ("message", `Assoc [
          ("content", `List [
            `Assoc [("text", `String "block1")];
            `Assoc [("text", `String "block2")];
          ])
        ]);
      ];
    ]);
  ]) in
  match parse_openai_response_result json_str with
  | Ok resp ->
    (match resp.content with
     | [Text t] -> t = "block1block2"
     | _ -> false)
  | Error _ -> false

let%test "parse_openai_response_result with reasoning_content" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "c1");
    ("model", `String "deepseek-r1");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "stop");
        ("message", `Assoc [
          ("content", `String "answer");
          ("reasoning_content", `String "I thought about it");
        ]);
      ];
    ]);
  ]) in
  match parse_openai_response_result json_str with
  | Ok resp ->
    List.exists (function Thinking _ -> true | _ -> false) resp.content
  | Error _ -> false

let%test "parse_openai_response_result JSON list wrapping" =
  let inner = `Assoc [
    ("id", `String "c1");
    ("model", `String "m");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "stop");
        ("message", `Assoc [("content", `String "ok")]);
      ];
    ]);
  ] in
  let json_str = Yojson.Safe.to_string (`List [inner]) in
  match parse_openai_response_result json_str with
  | Ok resp -> resp.id = "c1"
  | Error _ -> false

let%test "parse_openai_response_result error without message" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("error", `Assoc []);
  ]) in
  match parse_openai_response_result json_str with
  | Error msg -> msg = "Unknown API error"
  | Ok _ -> false

let%test "usage_of_openai_json prompt_tokens_details null" =
  let json = `Assoc [
    ("usage", `Assoc [
      ("prompt_tokens", `Int 50);
      ("completion_tokens", `Int 25);
      ("prompt_tokens_details", `Null);
    ]);
  ] in
  match usage_of_openai_json json with
  | Some u -> u.cache_read_input_tokens = 0
  | None -> false

let%test "openai_content_parts_of_blocks image block" =
  let blocks = [
    Image { media_type = "image/png"; data = "abc"; source_type = "base64" };
  ] in
  let result = openai_content_parts_of_blocks blocks in
  List.length result = 1

let%test "openai_content_parts_of_blocks document block" =
  let blocks = [
    Document { media_type = "application/pdf"; data = "abc"; source_type = "base64" };
  ] in
  let result = openai_content_parts_of_blocks blocks in
  List.length result = 1

let%test "openai_content_parts_of_blocks audio block" =
  let blocks = [
    Audio { media_type = "audio/wav"; data = "abc"; source_type = "base64" };
  ] in
  let result = openai_content_parts_of_blocks blocks in
  List.length result = 1

let%test "openai_content_parts_of_blocks redacted thinking filtered" =
  let blocks = [
    RedactedThinking "secret";
    Text "visible";
  ] in
  let result = openai_content_parts_of_blocks blocks in
  List.length result = 1

let%test "openai_content_parts_of_blocks tool_result filtered" =
  let blocks = [
    ToolResult { tool_use_id = "t1"; content = "result"; is_error = false; json = None };
  ] in
  openai_content_parts_of_blocks blocks = []

let%test "build_request includes tool_choice for model with supports_tool_choice=true" =
  let config = Provider_config.make ~kind:OpenAI_compat ~model_id:"gpt-4o"
      ~base_url:"http://localhost" ~tool_choice:Any () in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "tool_choice" |> to_string = "required"

let%test "build_request includes tool_choice for unknown model (backward compat)" =
  let config = Provider_config.make ~kind:OpenAI_compat ~model_id:"mystery-xyz-v1"
      ~base_url:"http://localhost" ~tool_choice:Any () in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "tool_choice" |> to_string = "required"

let%test "build_request omits tool_choice when tool_choice=None" =
  let config = Provider_config.make ~kind:OpenAI_compat ~model_id:"gpt-4o"
      ~base_url:"http://localhost" () in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  match json with
  | `Assoc fields -> not (List.exists (fun (k, _) -> k = "tool_choice") fields)
  | _ -> false
