(** Lightweight provider configuration for standalone LLM calls.
    @since 0.46.0 *)

type provider_kind =
  | Anthropic
  | OpenAI_compat
  | Ollama
  | Gemini
  | Glm
  | Claude_code
  | Gemini_cli
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
      | OpenAI_compat -> "/v1/chat/completions"
      | Ollama -> "/api/chat"
      | Gemini -> ""
      | Glm -> "/chat/completions"
      | Claude_code | Gemini_cli | Codex_cli -> ""
  in
  { kind; model_id; base_url; api_key; headers; request_path;
    max_tokens; max_context; temperature; top_p; top_k; min_p;
    system_prompt; enable_thinking; thinking_budget; clear_thinking;
    tool_stream;
    tool_choice; disable_parallel_tool_use; response_format; output_schema;
    cache_system_prompt; supports_tool_choice_override }

(** Lowercase string representation of the wire-format kind.
    Exhaustive match: adding a new variant triggers a compile error.
    @since 0.100.0 *)
let string_of_provider_kind = function
  | Anthropic -> "anthropic"
  | OpenAI_compat -> "openai_compat"
  | Ollama -> "ollama"
  | Gemini -> "gemini"
  | Glm -> "glm"
  | Claude_code -> "claude_code"
  | Gemini_cli -> "gemini_cli"
  | Codex_cli -> "codex_cli"

(** Canonical inverse of {!string_of_provider_kind}.

    Accepts every lowercase form produced by {!string_of_provider_kind} plus
    the documented legacy aliases used by cascade configs and downstream
    callers:
    - [claude]  -> [Anthropic]
    - [openai]  -> [OpenAI_compat]
    - [llama]   -> [Ollama]

    The match is case-insensitive; leading and trailing whitespace is
    trimmed. Returns [None] for any other input so callers can fail fast
    instead of silently falling back to a default provider.
    @since 0.165.0 *)
let provider_kind_of_string raw =
  match String.lowercase_ascii (String.trim raw) with
  | "anthropic" | "claude" -> Some Anthropic
  | "openai_compat" | "openai" -> Some OpenAI_compat
  | "ollama" | "llama" -> Some Ollama
  | "gemini" -> Some Gemini
  | "glm" -> Some Glm
  | "claude_code" -> Some Claude_code
  | "gemini_cli" -> Some Gemini_cli
  | "codex_cli" -> Some Codex_cli
  | _ -> None

(** Hand-written serializers for [provider_kind]. We do not use
    [\[@@deriving show, yojson\]] because the default generated forms would
    emit the capitalised constructor name (["Anthropic"]) rather than the
    wire-format lowercase produced by {!string_of_provider_kind}
    (["anthropic"]). Keeping the wire format stable matters when records
    embedding this type (for example [Types.inference_telemetry]) are
    serialised to disk / over the network. *)

let pp_provider_kind fmt k =
  Format.pp_print_string fmt (string_of_provider_kind k)

let show_provider_kind = string_of_provider_kind

let provider_kind_to_yojson (k : provider_kind) : Yojson.Safe.t =
  `String (string_of_provider_kind k)

let provider_kind_of_yojson
    (json : Yojson.Safe.t) :
  provider_kind Ppx_deriving_yojson_runtime.error_or =
  match json with
  | `String s ->
    (match provider_kind_of_string s with
     | Some k -> Ok k
     | None ->
       Error (Printf.sprintf "provider_kind: unknown value %S" s))
  | _ -> Error "provider_kind: expected JSON string"

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
      | Claude_code | Gemini_cli | Codex_cli ->
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
