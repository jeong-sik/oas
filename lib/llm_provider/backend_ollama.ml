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
    @ List.concat_map Backend_openai_serialize.ollama_messages_of_message messages
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

     We default to -1 (permanent) so the caller's pinned model stays
     resident. Override via OAS_OLLAMA_KEEP_ALIVE env var. Accepted values:
     - integer seconds: "-1", "0", "3600" → sent as [`Int n]
     - duration strings: "5m", "30m", "24h", "-1m" → sent as [`String v]

     Wire format matters: Ollama parses [keep_alive] in two ways depending
     on JSON type. Integer [-1] is the documented sentinel for "keep
     forever" and is always accepted. A string value goes through Go's
     [time.ParseDuration], which requires a unit suffix — [time.ParseDuration "-1"]
     fails with "missing unit in duration". Sending the plain string "-1"
     therefore produces [Invalid request: time: missing unit in duration "-1"]
     and every keeper turn errors out in <2s.

     Empirical rationale:
     - 2026-04-11 incident 1: a 35b-a3b model was evicted ~every 30 min
       because each consumer turn reset keep_alive to the 5m default.
     - 2026-04-11 incident 2: after pinning keep_alive=-1 (PR #813), every
       ollama request failed with the duration parse error above because
       -1 was serialized as [`String "-1"]. This fix sends an integer for
       parseable values and a duration string otherwise. *)
  let keep_alive_raw =
    match Sys.getenv_opt "OAS_OLLAMA_KEEP_ALIVE" with
    | Some v when String.trim v <> "" -> String.trim v
    | _ -> "-1"
  in
  let keep_alive_json : Yojson.Safe.t =
    match int_of_string_opt keep_alive_raw with
    | Some n -> `Int n
    | None -> `String keep_alive_raw
  in
  let body = ("keep_alive", keep_alive_json) :: body in

  let body =
    match config.output_schema, config.response_format with
    | Some schema, _ -> ("format", schema) :: body
    | None, Types.JsonSchema schema -> ("format", schema) :: body
    | None, Types.JsonMode -> ("format", `String "json") :: body
    | None, Types.Off -> body
  in

  let body = match tools with
    | [] -> body
    | ts ->
        ("tools", `List (List.map Backend_openai_serialize.build_openai_tool_json ts)) :: body
  in

  (* Sampling parameters go inside Ollama's "options" object.

     top_k / min_p are now capability-gated — not because native
     Ollama rejects them (its Options struct has both, llama.cpp
     samplers support them), but to mirror the #830/#831 contract
     across every OAS serializer so a future capability record that
     lowers either flag actually takes effect everywhere in the
     request-build pipeline.

     For the default ollama_capabilities (inherited from
     openai_chat_extended_capabilities) both flags are true, so
     behaviour is byte-identical for the common path. The gate only
     fires when an operator explicitly sets [supports_min_p = false]
     or [supports_top_k = false] for a specific Ollama variant — at
     which point the one-shot WARN from Backend_openai also fires. *)
  let caps =
    match Capabilities.for_model_id config.model_id with
    | Some c -> c
    | None -> Capabilities.ollama_capabilities
  in
  let options = ref [] in
  (let mt = Option.value ~default:4096 config.max_tokens in
   options := ("num_predict", `Int mt) :: !options);
  (match config.temperature with
   | Some t -> options := ("temperature", `Float t) :: !options
   | None -> ());
  (match config.top_p with
   | Some p -> options := ("top_p", `Float p) :: !options
   | None -> ());
  (match config.top_k with
   | Some k when caps.supports_top_k ->
     options := ("top_k", `Int k) :: !options
   | Some _ ->
     Backend_openai.warn_capability_drop
       ~model_id:config.model_id ~field:"top_k"
   | None -> ());
  (match config.min_p with
   | Some p when caps.supports_min_p ->
     options := ("min_p", `Float p) :: !options
   | Some _ ->
     Backend_openai.warn_capability_drop
       ~model_id:config.model_id ~field:"min_p"
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
    (* Ollama reports durations in nanoseconds. Surface them as
       inference_timings so downstream can distinguish hardware
       decode rate from wall-clock tok/s. *)
    let timings =
      let prompt_n = json |> member "prompt_eval_count" |> to_int_option in
      let prompt_ns = json |> member "prompt_eval_duration" |> to_int_option in
      let predicted_n = json |> member "eval_count" |> to_int_option in
      let predicted_ns = json |> member "eval_duration" |> to_int_option in
      let any_set =
        Option.is_some prompt_n || Option.is_some prompt_ns
        || Option.is_some predicted_n || Option.is_some predicted_ns
      in
      if not any_set then None
      else
        let ms_of_ns ns_opt =
          Option.map (fun ns -> float_of_int ns /. 1e6) ns_opt
        in
        let per_second n_opt ns_opt =
          match n_opt, ns_opt with
          | Some n, Some ns when ns > 0 ->
              Some (float_of_int n /. (float_of_int ns /. 1e9))
          | _ -> None
        in
        Some
          { Types.prompt_n
          ; prompt_ms = ms_of_ns prompt_ns
          ; prompt_per_second = per_second prompt_n prompt_ns
          ; predicted_n
          ; predicted_ms = ms_of_ns predicted_ns
          ; predicted_per_second = per_second predicted_n predicted_ns
          ; cache_n = None
          }
    in
    let reasoning_tokens = None in
    Some { Types.system_fingerprint; timings; reasoning_tokens; request_latency_ms = 0;
            peak_memory_gb = None;
            provider_kind = None; reasoning_effort = None;
            canonical_model_id = None; effective_context_window = None;
            provider_internal_action_count = None }
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

let%test "build_request pins keep_alive=-1 as integer by default" =
  with_keep_alive_env "" (fun () ->
    let config = Provider_config.make
      ~kind:Ollama ~model_id:"qwen3.5:35b-a3b-nvfp4"
      ~base_url:"http://127.0.0.1:11434" () in
    let messages = [{ role = User; content = [Text "hi"]; name = None; tool_call_id = None ; metadata = []}] in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    (* Integer wire format: -1 as [`Int (-1)] avoids Ollama's
       [time.ParseDuration "-1"] failure ("missing unit in duration"). *)
    json |> member "keep_alive" |> to_int = -1)

let%test "build_request integer override sent as `Int" =
  with_keep_alive_env "3600" (fun () ->
    let config = Provider_config.make
      ~kind:Ollama ~model_id:"qwen3.5:35b-a3b-nvfp4"
      ~base_url:"http://127.0.0.1:11434" () in
    let messages = [{ role = User; content = [Text "hi"]; name = None; tool_call_id = None ; metadata = []}] in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "keep_alive" |> to_int = 3600)

let%test "build_request duration string override sent as `String" =
  with_keep_alive_env "30m" (fun () ->
    let config = Provider_config.make
      ~kind:Ollama ~model_id:"qwen3.5:35b-a3b-nvfp4"
      ~base_url:"http://127.0.0.1:11434" () in
    let messages = [{ role = User; content = [Text "hi"]; name = None; tool_call_id = None ; metadata = []}] in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "keep_alive" |> to_string = "30m")

let%test "build_request trims whitespace around override" =
  with_keep_alive_env "  -1m  " (fun () ->
    let config = Provider_config.make
      ~kind:Ollama ~model_id:"qwen3.5:35b-a3b-nvfp4"
      ~base_url:"http://127.0.0.1:11434" () in
    let messages = [{ role = User; content = [Text "hi"]; name = None; tool_call_id = None ; metadata = []}] in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "keep_alive" |> to_string = "-1m")

let%test "build_request whitespace-only env falls back to default integer" =
  with_keep_alive_env "   " (fun () ->
    let config = Provider_config.make
      ~kind:Ollama ~model_id:"qwen3.5:35b-a3b-nvfp4"
      ~base_url:"http://127.0.0.1:11434" () in
    let messages = [{ role = User; content = [Text "hi"]; name = None; tool_call_id = None ; metadata = []}] in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "keep_alive" |> to_int = -1)

let%test "parse_ollama_response populates timings from eval_count/eval_duration" =
  let json =
    {|{"model":"qwen3.5:35b-a3b-nvfp4","done":true,"done_reason":"stop",
       "message":{"role":"assistant","content":"hi"},
       "prompt_eval_count":100,"prompt_eval_duration":200000000,
       "eval_count":120,"eval_duration":2000000000}|}
  in
  match parse_ollama_response json with
  | Error _ -> false
  | Ok resp ->
    (match resp.telemetry with
     | Some { timings = Some t; _ } ->
       t.predicted_n = Some 120
       && (match t.predicted_per_second with
           | Some v -> abs_float (v -. 60.0) < 0.001
           | None -> false)
       && t.prompt_n = Some 100
       && (match t.prompt_per_second with
           | Some v -> abs_float (v -. 500.0) < 0.001
           | None -> false)
     | _ -> false)

let%test "parse_ollama_response guards zero eval_duration" =
  let json =
    {|{"model":"qwen3.5:35b-a3b-nvfp4","done":true,"done_reason":"stop",
       "message":{"role":"assistant","content":"hi"},
       "eval_count":10,"eval_duration":0}|}
  in
  match parse_ollama_response json with
  | Error _ -> false
  | Ok resp ->
    (match resp.telemetry with
     | Some { timings = Some t; _ } ->
       (* eval_count present → timings record exists,
          but predicted_per_second is None because duration is 0. *)
       t.predicted_n = Some 10 && t.predicted_per_second = None
     | _ -> false)

let%test "parse_ollama_response returns timings=None when no timing fields present" =
  let json =
    {|{"model":"qwen3.5:35b-a3b-nvfp4","done":true,"done_reason":"stop",
       "message":{"role":"assistant","content":"hi"}}|}
  in
  match parse_ollama_response json with
  | Error _ -> false
  | Ok resp ->
    (match resp.telemetry with
     | Some { timings = None; _ } -> true
     | _ -> false)
