(** Ollama native API request building and response parsing.

    Uses [/api/chat] endpoint with [think] parameter for thinking control,
    and [options] object for sampling parameters.

    @since 0.113.0 *)

open Types

(* ── Request building ────────────────────────────────── *)

(** Build Ollama native [/api/chat] request body.
    Key differences from OpenAI compat:
    - [think] parameter (boolean) instead of [chat_template_kwargs]
    - Sampling params go inside [options] object
    - [num_predict] instead of [max_tokens]
    - No [tool_choice] support *)
let build_request ?(stream=false) ~(config : Provider_config.t)
    ~(messages : message list) ?(tools : Yojson.Safe.t list = []) () =

  let provider_messages =
    (match config.system_prompt with
     | Some s when not (Api_common.string_is_blank s) ->
         [`Assoc [("role", `String "system"); ("content", `String (Utf8_sanitize.sanitize s))]]
     | _ -> [])
    @ List.concat_map Backend_openai_serialize.openai_messages_of_message messages
  in

  let body =
    [ ("model", `String config.model_id);
      ("messages", `List provider_messages) ]
  in

  (* think: false by default for Ollama to prevent thinking models from
     consuming all tokens in reasoning. Only enable when explicitly requested. *)
  let think = match config.enable_thinking with
    | Some true -> true
    | _ -> false
  in
  let body = ("think", `Bool think) :: body in

  (* Ollama defaults to stream=true, so always send explicit value *)
  let body = ("stream", `Bool stream) :: body in

  (* keep_alive: controls how long the model stays loaded in memory after
     the request. Ollama's default is 5 minutes, which causes models to be
     unloaded between keeper cycles and re-loaded on demand — slow and
     eviction-prone when other processes ping different models.

     We default to "-1" (permanent) so the caller's pinned model stays
     resident. Override via OAS_OLLAMA_KEEP_ALIVE env var ("5m", "0",
     "-1", duration strings — Ollama accepts any of these).

     Empirical rationale: 2026-04-11 incident where masc-mcp's 35b-a3b
     model was evicted ~every 30 min because each keeper turn reset
     keep_alive to the 5m default; concurrent probes from other processes
     then triggered swap-induced JSON truncation errors. *)
  let keep_alive_raw =
    match Sys.getenv_opt "OAS_OLLAMA_KEEP_ALIVE" with
    | Some v when String.trim v <> "" -> String.trim v
    | _ -> "-1"
  in
  (* Ollama accepts either a bare integer (seconds; -1 = forever, 0 =
     unload immediately) or a duration string ("30m", "5m0s", ...).
     The Go server parses string values via [time.ParseDuration], which
     REJECTS bare integers-as-strings like "-1" with
     [time: missing unit in duration "-1"] — the exact error we saw
     1149 times/day in masc-mcp after oas#813 landed.

     So serialise integer-parseable values as JSON ints (including the
     negative path), and everything else as strings.  Preserves
     compatibility for "30m"/"5m" etc. while keeping "-1" / "0" /
     "300" working. *)
  let keep_alive_json =
    match int_of_string_opt keep_alive_raw with
    | Some n -> `Int n
    | None -> `String keep_alive_raw
  in
  let body = ("keep_alive", keep_alive_json) :: body in

  let body = match tools with
    | [] -> body
    | ts ->
        ("tools", `List (List.map Backend_openai_serialize.build_openai_tool_json ts)) :: body
  in

  (* Sampling parameters go inside "options" object *)
  let options = ref [] in
  (match config.max_tokens with
   | n -> options := ("num_predict", `Int n) :: !options);
  (match config.temperature with
   | Some t -> options := ("temperature", `Float t) :: !options
   | None -> ());
  (match config.top_p with
   | Some p -> options := ("top_p", `Float p) :: !options
   | None -> ());
  (match config.top_k with
   | Some k -> options := ("top_k", `Int k) :: !options
   | None -> ());
  (match config.min_p with
   | Some p -> options := ("min_p", `Float p) :: !options
   | None -> ());
  let body = ("options", `Assoc !options) :: body in

  Yojson.Safe.to_string (`Assoc body)

(* ── Response parsing ────────────────────────────────── *)

let parse_ollama_response json_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string json_str in

  match json |> member "error" with
  | `String s -> Error s
  | `Assoc _ as err -> Error (Yojson.Safe.to_string err)
  | _ ->

  let message = json |> member "message" in

  let text_content, tool_blocks, thinking_blocks =
    match message with
    | `Assoc _ ->
        let txt =
          match message |> member "content" with
          | `String s -> s
          | _ -> ""
        in
        let tools =
          match message |> member "tool_calls" with
          | `List calls ->
              List.filter_map
                (fun tc ->
                  try
                    let fn = tc |> member "function" in
                    let arguments =
                      fn |> member "arguments" |> to_string_option |> Option.value ~default:"{}"
                    in
                    Some
                      (ToolUse
                         { id = tc |> member "id" |> to_string_option |> Option.value ~default:"ollama-call";
                           name = fn |> member "name" |> to_string;
                           input = Api_common.json_of_string_or_raw arguments })
                  with Yojson.Safe.Util.Type_error _ | Yojson.Safe.Util.Undefined _ | Yojson.Json_error _ -> None)
                calls
          | _ -> []
        in
        let thinking =
          match message |> member "thinking" with
          | `String s when not (Api_common.string_is_blank s) ->
              [Thinking { thinking_type = "thinking"; content = s }]
          | _ -> []
        in
        (txt, tools, thinking)
    | _ -> ("", [], [])
  in

  let done_reason =
    json |> member "done_reason" |> to_string_option |> Option.value ~default:"stop"
  in

  let stop_reason =
    match String.lowercase_ascii done_reason with
    | "tool_calls" when tool_blocks <> [] -> StopToolUse
    | "length" -> MaxTokens
    | "stop" -> EndTurn
    | _other when tool_blocks <> [] -> StopToolUse
    | other -> Unknown other
  in

  let input_tokens =
    json |> member "prompt_eval_count" |> to_int_option |> Option.value ~default:0
  in
  let output_tokens =
    json |> member "eval_count" |> to_int_option |> Option.value ~default:0
  in

  let usage =
    if input_tokens = 0 && output_tokens = 0 then None
    else Some {
      input_tokens;
      output_tokens;
      cache_creation_input_tokens = 0;
      cache_read_input_tokens = 0;
      cost_usd = None;
    }
  in

  let telemetry =
    let system_fingerprint = None in
    let timings = None in
    let reasoning_tokens = None in
    Some { Types.system_fingerprint; timings; reasoning_tokens; request_latency_ms = 0;
            provider_kind = None; reasoning_effort = None;
            canonical_model_id = None; effective_context_window = None }
  in

  Ok {
    id = json |> member "model" |> to_string_option |> Option.value ~default:"ollama";
    model = json |> member "model" |> to_string_option |> Option.value ~default:"";
    stop_reason;
    content = thinking_blocks @ (if Api_common.string_is_blank text_content then [] else [Text text_content]) @ tool_blocks;
    usage;
    telemetry;
  }

(* ── Inline tests ────────────────────────────────── *)

[@@@coverage off]

(** Run [f] with the [OAS_OLLAMA_KEEP_ALIVE] env var set to [value], then
    restore the caller's original setting. Guaranteed restore on exception. *)
let with_keep_alive_env value f =
  let orig = Sys.getenv_opt "OAS_OLLAMA_KEEP_ALIVE" in
  let restore () =
    match orig with
    | None -> Unix.putenv "OAS_OLLAMA_KEEP_ALIVE" ""
    | Some v -> Unix.putenv "OAS_OLLAMA_KEEP_ALIVE" v
  in
  Fun.protect ~finally:restore (fun () ->
    Unix.putenv "OAS_OLLAMA_KEEP_ALIVE" value;
    f ())

let%test "build_request pins keep_alive=-1 as JSON int by default" =
  with_keep_alive_env "" (fun () ->
    let config = Provider_config.make
      ~kind:Ollama ~model_id:"qwen3.5:35b-a3b-nvfp4"
      ~base_url:"http://127.0.0.1:11434" () in
    let messages = [{ role = User; content = [Text "hi"]; name = None; tool_call_id = None }] in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "keep_alive" |> to_int = -1)

let%test "build_request honors OAS_OLLAMA_KEEP_ALIVE duration string override" =
  with_keep_alive_env "30m" (fun () ->
    let config = Provider_config.make
      ~kind:Ollama ~model_id:"qwen3.5:35b-a3b-nvfp4"
      ~base_url:"http://127.0.0.1:11434" () in
    let messages = [{ role = User; content = [Text "hi"]; name = None; tool_call_id = None }] in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "keep_alive" |> to_string = "30m")

let%test "build_request honors OAS_OLLAMA_KEEP_ALIVE integer override as JSON int" =
  with_keep_alive_env "300" (fun () ->
    let config = Provider_config.make
      ~kind:Ollama ~model_id:"qwen3.5:35b-a3b-nvfp4"
      ~base_url:"http://127.0.0.1:11434" () in
    let messages = [{ role = User; content = [Text "hi"]; name = None; tool_call_id = None }] in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "keep_alive" |> to_int = 300)

let%test "build_request whitespace-only env falls back to default (-1 as int)" =
  with_keep_alive_env "   " (fun () ->
    let config = Provider_config.make
      ~kind:Ollama ~model_id:"qwen3.5:35b-a3b-nvfp4"
      ~base_url:"http://127.0.0.1:11434" () in
    let messages = [{ role = User; content = [Text "hi"]; name = None; tool_call_id = None }] in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "keep_alive" |> to_int = -1)
