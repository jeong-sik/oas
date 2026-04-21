(** Lightweight provider configuration for standalone LLM calls.
    @since 0.46.0 *)

(** Re-exported from {!Provider_kind} so existing callers
    ([Provider_config.Anthropic], [Provider_config.string_of_provider_kind],
    …) keep working. The underlying type now lives in {!Provider_kind} so it
    can be shared with {!Types} without creating a module dependency cycle. *)
type provider_kind = Provider_kind.t =
  | Anthropic
  | Kimi
  | OpenAI_compat
  | Ollama
  | Gemini
  | Glm
  | Claude_code
  | Gemini_cli
  | Kimi_cli
  | Codex_cli

type t = {
  kind: provider_kind;
  model_id: string;
  base_url: string;
  api_key: string;
  headers: (string * string) list;
  request_path: string;
  max_tokens: int option;
  max_context: int option;
  temperature: float option;
  top_p: float option;
  top_k: int option;
  min_p: float option;
  system_prompt: string option;
  enable_thinking: bool option;
  thinking_budget: int option;
  clear_thinking: bool option;
  tool_stream: bool;
  tool_choice: Types.tool_choice option;
  disable_parallel_tool_use: bool;
  response_format: Types.response_format;
  output_schema: Yojson.Safe.t option;
  cache_system_prompt: bool;
  supports_tool_choice_override: bool option;
}

let make ~kind ~model_id ~base_url
    ?(api_key="") ?(headers=[("Content-Type", "application/json")])
    ?request_path ?max_tokens ?max_context
    ?temperature ?top_p ?top_k ?min_p
    ?system_prompt ?enable_thinking ?thinking_budget
    ?clear_thinking ?(tool_stream=false)
    ?tool_choice ?(disable_parallel_tool_use=false)
    ?response_format
    ?(response_format_json=false)
    ?output_schema
    ?(cache_system_prompt=false)
    ?supports_tool_choice_override () =
  let response_format =
    match response_format, output_schema with
    | Some value, _ -> value
    | None, Some schema -> Types.JsonSchema schema
    | None, None -> Types.response_format_of_json_mode response_format_json
  in
  let output_schema =
    match output_schema, response_format with
    | Some schema, _ -> Some schema
    | None, Types.JsonSchema schema -> Some schema
    | None, Types.JsonMode | None, Types.Off -> None
  in
  let request_path = match request_path with
    | Some p -> p
    | None -> match kind with
      | Anthropic -> "/v1/messages"
      | Kimi -> "/v1/messages"
      | OpenAI_compat -> "/v1/chat/completions"
      | Ollama -> "/api/chat"
      | Gemini -> ""
      | Glm -> "/chat/completions"
      | Claude_code | Gemini_cli | Kimi_cli | Codex_cli -> ""
  in
  { kind; model_id; base_url; api_key; headers; request_path;
    max_tokens; max_context; temperature; top_p; top_k; min_p;
    system_prompt; enable_thinking; thinking_budget; clear_thinking;
    tool_stream;
    tool_choice; disable_parallel_tool_use; response_format; output_schema;
    cache_system_prompt; supports_tool_choice_override }

(** Helpers for [provider_kind]. Implementations live in {!Provider_kind};
    these re-exports keep the call-site [Provider_config.*] namespace
    unchanged while the underlying type is hoisted to a shared module. *)
let string_of_provider_kind = Provider_kind.to_string
let provider_kind_of_string = Provider_kind.of_string
let all_provider_kinds = Provider_kind.all
let default_api_key_env = Provider_kind.default_api_key_env
let is_subprocess_cli = Provider_kind.is_subprocess_cli
let pp_provider_kind = Provider_kind.pp
let show_provider_kind = Provider_kind.show
let provider_kind_to_yojson = Provider_kind.to_yojson
let provider_kind_of_yojson = Provider_kind.of_yojson

(** Map thinking configuration to reasoning_effort string.
    Four levels: "none", "low" (≤2048), "medium" (≤8192), "high" (>8192).
    Shared by Ollama backends and api_openai request building.
    @since 0.114.0 *)
let effort_of_thinking_config ~(enable_thinking : bool option)
    ~(thinking_budget : int option) : string =
  match enable_thinking with
  | Some false | None -> "none"
  | Some true ->
      match thinking_budget with
      | Some n when n <= 0 -> "none"
      | Some n when n <= 2048 -> "low"
      | Some n when n <= 8192 -> "medium"
      | Some _ -> "high"
      | None -> "medium"

(** Compute reasoning_effort for a provider config.
    Returns [None] for non-Ollama providers.
    @since 0.114.0 *)
let reasoning_effort_of_config (config : t) : string option =
  match config.kind with
  | Ollama -> Some (effort_of_thinking_config
                      ~enable_thinking:config.enable_thinking
                      ~thinking_budget:config.thinking_budget)
  | _ -> None

let structured_output_name_of_schema (schema : Yojson.Safe.t) : string =
  let default_name = "structured_output" in
  let raw_name =
    match schema with
    | `Assoc fields ->
        (match List.assoc_opt "title" fields with
         | Some (`String s) when String.trim s <> "" -> s
         | _ -> default_name)
    | _ -> default_name
  in
  let normalized =
    let buf = Buffer.create (String.length raw_name) in
    let last_was_sep = ref false in
    let push_sep () =
      if Buffer.length buf > 0 && not !last_was_sep then begin
        Buffer.add_char buf '_';
        last_was_sep := true
      end
    in
    String.iter (fun ch ->
      match ch with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
          Buffer.add_char buf (Char.lowercase_ascii ch);
          last_was_sep := false
      | '_' | '-' ->
          Buffer.add_char buf ch;
          last_was_sep := true
      | _ -> push_sep ()
    ) raw_name;
    Buffer.contents buf
  in
  let rec trim_bounds s =
    let len = String.length s in
    if len = 0 then default_name
    else
      let first = s.[0] and last = s.[len - 1] in
      if first = '_' || first = '-' then
        trim_bounds (String.sub s 1 (len - 1))
      else if last = '_' || last = '-' then
        trim_bounds (String.sub s 0 (len - 1))
      else s
  in
  let trimmed = trim_bounds normalized in
  if trimmed = "" then default_name else trimmed

let openai_host_supports_output_schema base_url =
  match Uri.of_string base_url |> Uri.host with
  | Some host -> String.lowercase_ascii host = "api.openai.com"
  | None -> false

let validate_output_schema_request (config : t) =
  match config.output_schema with
  | None -> Ok ()
  | Some _ ->
      match config.kind with
      | Gemini | Anthropic | Ollama -> Ok ()
      | Kimi ->
          Error
            "Kimi direct API native json_schema output is not verified yet in OAS"
      | OpenAI_compat ->
          let caps =
            match Capabilities.for_model_id config.model_id with
            | Some c -> c
            | None -> Capabilities.default_capabilities
          in
          if not caps.supports_structured_output then
            Error
              (Printf.sprintf
                 "model %s does not advertise native structured output"
                 config.model_id)
          else if openai_host_supports_output_schema config.base_url then
            Ok ()
          else
            Error
              (Printf.sprintf
                 "native structured output is only wired for official OpenAI hosts, got %s"
                 config.base_url)
      | Glm ->
          Error "GLM is currently wired for JSON mode only; native json_schema is not enabled"
      | Claude_code | Gemini_cli | Kimi_cli | Codex_cli ->
          Error
            (Printf.sprintf "%s does not expose provider-native structured output in OAS"
               (string_of_provider_kind config.kind))

let has_host_prefix ~url ~prefix =
  let prefix_len = String.length prefix in
  String.length url >= prefix_len
  && String.sub url 0 prefix_len = prefix
  &&
  let next_index = prefix_len in
  String.length url = prefix_len
  || match url.[next_index] with
     | ':' | '/' | '?' | '#' -> true
     | _ -> false

let is_local (config : t) =
  let url = String.lowercase_ascii (String.trim config.base_url) in
  has_host_prefix ~url ~prefix:Constants.Endpoints.local_prefix
  || has_host_prefix ~url ~prefix:Constants.Endpoints.localhost_prefix
