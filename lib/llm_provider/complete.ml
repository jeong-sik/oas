(** Standalone LLM completion: build -> HTTP -> parse.

    Self-contained in llm_provider -- no agent_sdk dependency.
    Both OAS and MASC can call these functions directly.

    @since 0.46.0  Sync completion
    @since 0.53.0  Streaming, retry, cascade
    @since 0.54.0  Optional cache + metrics hooks *)

(* ── Internal: timed HTTP completion ──────────────────── *)

(** Construct the URL for a Gemini API call.
    Sync: [base_url/models/model_id:generateContent?key=api_key]
    Stream: [base_url/models/model_id:streamGenerateContent?key=api_key&alt=sse]
    When api_key is empty (Vertex AI), the [?key=] param is omitted. *)
let gemini_url ~(config : Provider_config.t) ~stream =
  let method_name = if stream then "streamGenerateContent" else "generateContent" in
  let base = Printf.sprintf "%s/models/%s:%s"
      config.base_url config.model_id method_name
  in
  let params =
    (if config.api_key <> "" then [Printf.sprintf "key=%s" config.api_key] else [])
    @ (if stream then ["alt=sse"] else [])
  in
  match params with
  | [] -> base
  | ps -> base ^ "?" ^ String.concat "&" ps

(** Provider-aware sampling parameter defaults.
    Local providers get min_p=0.05 (2026 llama.cpp standard).
    Anthropic gets no top_p (incompatible with temperature).
    Explicit agent_config values always take priority (overlay pattern). *)
type sampling_defaults = {
  default_min_p : float option;
  default_top_p : float option;
  default_top_k : int option;
}

let provider_sampling_defaults (kind : Provider_config.provider_kind) : sampling_defaults =
  match kind with
  | Provider_config.OpenAI_compat ->
    { default_min_p = Some 0.05; default_top_p = None; default_top_k = None }
  | Provider_config.Anthropic ->
    { default_min_p = None; default_top_p = None; default_top_k = None }
  | Provider_config.Gemini ->
    { default_min_p = None; default_top_p = None; default_top_k = None }
  | Provider_config.Glm ->
    { default_min_p = None; default_top_p = None; default_top_k = None }
  | Provider_config.Claude_code ->
    { default_min_p = None; default_top_p = None; default_top_k = None }

(** Apply provider defaults to a config, preserving explicit values (overlay pattern).
    Only fills in None fields; explicit values are never overwritten. *)
let apply_sampling_defaults (config : Provider_config.t) : Provider_config.t =
  let defaults = provider_sampling_defaults config.kind in
  { config with
    min_p = (match config.min_p with Some _ -> config.min_p | None -> defaults.default_min_p);
    top_p = (match config.top_p with Some _ -> config.top_p | None -> defaults.default_top_p);
    top_k = (match config.top_k with Some _ -> config.top_k | None -> defaults.default_top_k);
  }

let complete_http ~sw ~net ~(config : Provider_config.t)
    ~(messages : Types.message list) ~tools =
  if config.kind = Provider_config.Claude_code then
    (Error (Http_client.NetworkError {
       message = "Claude_code provider requires a transport (use Transport_claude_code.create)" }),
     0)
  else
  let config = apply_sampling_defaults config in
  let body_str = match config.kind with
    | Provider_config.Anthropic ->
        Backend_anthropic.build_request ~config ~messages ~tools ()
    | Provider_config.OpenAI_compat ->
        Backend_openai.build_request ~config ~messages ~tools ()
    | Provider_config.Gemini ->
        Backend_gemini.build_request ~config ~messages ~tools ()
    | Provider_config.Glm ->
        Backend_glm.build_request ~config ~messages ~tools ()
    | Provider_config.Claude_code -> assert false (* guarded above *)
  in
  let url = match config.kind with
    | Provider_config.Gemini -> gemini_url ~config ~stream:false
    | _ -> config.base_url ^ config.request_path
  in
  let t0 = Unix.gettimeofday () in
  let result =
    match Http_client.post_sync ~sw ~net ~url
            ~headers:config.headers ~body:body_str with
    | Error _ as e -> e
    | Ok (code, body) ->
        if code >= 200 && code < 300 then
          let response = match config.kind with
            | Provider_config.Anthropic ->
                Backend_anthropic.parse_response
                  (Yojson.Safe.from_string body)
            | Provider_config.OpenAI_compat ->
                Backend_openai.parse_openai_response body
            | Provider_config.Gemini ->
                Backend_gemini.parse_response
                  (Yojson.Safe.from_string body)
            | Provider_config.Glm ->
                Backend_glm.parse_response body
            | Provider_config.Claude_code -> assert false
          in
          Ok response
        else
          Error (Http_client.HttpError { code; body })
  in
  let latency_ms = int_of_float ((Unix.gettimeofday () -. t0) *. 1000.0) in
  (result, latency_ms)

(* ── Sync completion ─────────────────────────────────── *)

let complete ~sw ~net ?(transport : Llm_transport.t option)
    ~(config : Provider_config.t)
    ~(messages : Types.message list) ?(tools=[])
    ?(cache : Cache.t option) ?(metrics : Metrics.t option) () =
  let m = match metrics with Some m -> m | None -> Metrics.noop in
  let model_id = config.model_id in
  (* Cache lookup *)
  (* Compute fingerprint once; reuse for both lookup and store *)
  let cache_key = match cache with
    | Some _ -> Some (Cache.request_fingerprint ~config ~messages ~tools ())
    | None -> None
  in
  let cached = match cache, cache_key with
    | Some c, Some key ->
        (match c.get ~key with
         | Some json ->
             (match Cache.response_of_json json with
              | Some resp ->
                  m.on_cache_hit ~model_id;
                  Some (Ok resp)
              | None ->
                  m.on_cache_miss ~model_id;
                  None)
         | None ->
             m.on_cache_miss ~model_id;
             None)
    | _, _ -> None
  in
  match cached with
  | Some result -> result
  | None ->
      m.on_request_start ~model_id;
      let { Llm_transport.response = result; latency_ms } =
        match transport with
        | Some t ->
          t.complete_sync { Llm_transport.config; messages; tools }
        | None ->
          let (resp, lat) = complete_http ~sw ~net ~config ~messages ~tools in
          { Llm_transport.response = resp; latency_ms = lat }
      in
      (match result with
       | Ok resp ->
           m.on_request_end ~model_id ~latency_ms;
           (* Cache store — reuse pre-computed key *)
           (match cache, cache_key with
            | Some c, Some key ->
                let json = Cache.response_to_json resp in
                (try c.set ~key ~ttl_sec:300 json
                 with Eio.Io _ | Sys_error _ -> ())
            | _, _ -> ());
           Ok resp
       | Error err ->
           let err_str = match err with
             | Http_client.HttpError { code; _ } ->
                 Printf.sprintf "HTTP %d" code
             | Http_client.NetworkError { message } -> message
           in
           m.on_error ~model_id ~error:err_str;
           Error err)

(* ── Retry ───────────────────────────────────────────── *)

type retry_config = {
  max_retries: int;
  initial_delay_sec: float;
  max_delay_sec: float;
  backoff_multiplier: float;
}

let default_retry_config = {
  max_retries = 3;
  initial_delay_sec = 1.0;
  max_delay_sec = 30.0;
  backoff_multiplier = 2.0;
}

let is_retryable = function
  | Http_client.HttpError { code; _ } ->
      code = 429 || code = 500 || code = 502 || code = 503 || code = 529
  | Http_client.NetworkError _ -> true

let complete_with_retry ~sw ~net ?transport ~clock
    ~(config : Provider_config.t)
    ~(messages : Types.message list) ?(tools=[])
    ?(retry_config=default_retry_config)
    ?cache ?metrics () =
  (* Jitter: randomize delay by 0.5x-1.5x to prevent thundering herd *)
  let jittered delay =
    let factor = 0.5 +. Random.float 1.0 in
    delay *. factor
  in
  let rec attempt n delay =
    match complete ~sw ~net ?transport ~config ~messages ~tools ?cache ?metrics () with
    | Ok _ as success -> success
    | Error err when is_retryable err && n < retry_config.max_retries ->
        Eio.Time.sleep clock (jittered delay);
        let next_delay =
          Float.min
            (delay *. retry_config.backoff_multiplier)
            retry_config.max_delay_sec
        in
        attempt (n + 1) next_delay
    | Error _ as fail -> fail
  in
  attempt 0 retry_config.initial_delay_sec

(* ── Cascade: multi-provider failover ────────────────── *)

type cascade = {
  primary: Provider_config.t;
  fallbacks: Provider_config.t list;
}

let complete_cascade ~sw ~net ?transport ?clock ?retry_config
    ~(cascade : cascade)
    ~(messages : Types.message list) ?(tools=[])
    ?cache ?metrics () =
  let m = match metrics with Some m -> m | None -> Metrics.noop in
  let try_provider cfg =
    match clock with
    | Some clock ->
        complete_with_retry ~sw ~net ?transport ~clock ~config:cfg
          ~messages ~tools ?retry_config ?cache ?metrics ()
    | None ->
        complete ~sw ~net ?transport ~config:cfg ~messages ~tools ?cache ?metrics ()
  in
  match try_provider cascade.primary with
  | Ok _ as success -> success
  | Error err ->
      if is_retryable err then
        let primary_id = cascade.primary.model_id in
        let rec try_fallbacks last_err = function
          | [] -> Error last_err
          | (fb : Provider_config.t) :: rest ->
              let err_str = match last_err with
                | Http_client.HttpError { code; _ } ->
                    Printf.sprintf "HTTP %d" code
                | Http_client.NetworkError { message } -> message
              in
              m.on_cascade_fallback
                ~from_model:primary_id ~to_model:fb.model_id ~reason:err_str;
              match try_provider fb with
              | Ok _ as success -> success
              | Error err2 ->
                  if is_retryable err2 then try_fallbacks err2 rest
                  else Error err2
        in
        try_fallbacks err cascade.fallbacks
      else Error err

(* ── Streaming ───────────────────────────────────────── *)

(* Re-export stream accumulator for backward compatibility *)
include Complete_stream_acc

(* Internal: HTTP-specific streaming implementation. *)
let complete_stream_http ~sw ~net ~(config : Provider_config.t)
    ~(messages : Types.message list) ~tools
    ~(on_event : Types.sse_event -> unit) =
  if config.kind = Provider_config.Claude_code then
    Error (Http_client.NetworkError {
      message = "Claude_code provider requires a transport (use Transport_claude_code.create)" })
  else
  let config = apply_sampling_defaults config in
  let body_str = match config.kind with
    | Provider_config.Anthropic ->
        Backend_anthropic.build_request ~stream:true ~config ~messages ~tools ()
    | Provider_config.OpenAI_compat ->
        Backend_openai.build_request ~stream:true ~config ~messages ~tools ()
    | Provider_config.Gemini ->
        Backend_gemini.build_request ~stream:true ~config ~messages ~tools ()
    | Provider_config.Glm ->
        Backend_glm.build_request ~stream:true ~config ~messages ~tools ()
    | Provider_config.Claude_code -> assert false
  in
  let url = match config.kind with
    | Provider_config.Gemini -> gemini_url ~config ~stream:true
    | _ -> config.base_url ^ config.request_path
  in
  let body_with_stream = match config.kind with
    | Provider_config.Gemini -> body_str
    | _ -> Http_client.inject_stream_param body_str
  in
  match Http_client.post_stream ~sw ~net ~url
          ~headers:config.headers ~body:body_with_stream with
  | Error _ as e -> e
  | Ok reader ->
      let acc = create_stream_acc () in
      let openai_state = ref None in
      Http_client.read_sse ~reader ~on_data:(fun ~event_type data ->
        let events = match config.kind with
          | Provider_config.Anthropic ->
              (match Streaming.parse_sse_event event_type data with
               | Some evt -> [evt]
               | None -> [])
          | Provider_config.OpenAI_compat ->
              let state = match !openai_state with
                | Some s -> s
                | None ->
                    let s = Streaming.create_openai_stream_state () in
                    openai_state := Some s; s
              in
              (match Streaming.parse_openai_sse_chunk data with
               | Some chunk -> Streaming.openai_chunk_to_events state chunk
               | None -> [])
          | Provider_config.Gemini ->
              let state = match !openai_state with
                | Some s -> s
                | None ->
                    let s = Streaming.create_openai_stream_state () in
                    openai_state := Some s; s
              in
              (match Streaming.parse_gemini_sse_chunk data with
               | Some chunk -> Streaming.gemini_chunk_to_events state chunk
               | None -> [])
          | Provider_config.Glm ->
              (* GLM uses OpenAI-compatible SSE format *)
              let state = match !openai_state with
                | Some s -> s
                | None ->
                    let s = Streaming.create_openai_stream_state () in
                    openai_state := Some s; s
              in
              (match Backend_glm.parse_stream_chunk data with
               | Some chunk -> Streaming.openai_chunk_to_events state chunk
               | None -> [])
          | Provider_config.Claude_code -> []  (* guarded above *)
        in
        List.iter (fun evt ->
          on_event evt;
          accumulate_event acc evt
        ) events
      ) ();
      match finalize_stream_acc acc with
      | Ok resp -> Ok resp
      | Error msg ->
          Error (Http_client.NetworkError {
            message = Printf.sprintf "SSE stream error: %s" msg })

let complete_stream ~sw ~net ?(transport : Llm_transport.t option)
    ~(config : Provider_config.t)
    ~(messages : Types.message list) ?(tools=[])
    ~(on_event : Types.sse_event -> unit) () =
  match transport with
  | Some t ->
    t.complete_stream ~on_event { Llm_transport.config; messages; tools }
  | None ->
    complete_stream_http ~sw ~net ~config ~messages ~tools ~on_event

(* ── HTTP Transport constructor ─────────────────────── *)

let make_http_transport ~sw ~net : Llm_transport.t = {
  complete_sync = (fun (req : Llm_transport.completion_request) ->
    let (response, latency_ms) =
      complete_http ~sw ~net ~config:req.config
        ~messages:req.messages ~tools:req.tools
    in
    { Llm_transport.response; latency_ms });
  complete_stream = (fun ~on_event (req : Llm_transport.completion_request) ->
    complete_stream_http ~sw ~net ~config:req.config
      ~messages:req.messages ~tools:req.tools ~on_event);
}

(* ── Streaming Cascade ──────────────────────────── *)

(** Streaming cascade: try each provider in order, failover on
    connection/HTTP errors only. Once the SSE stream begins,
    events are emitted to [on_event] and the provider is committed.
    No mid-stream failover, no retry, no caching.

    @since 0.61.0 *)
let complete_stream_cascade ~sw ~net ?transport
    ~(cascade : cascade)
    ~(messages : Types.message list) ?(tools=[])
    ~(on_event : Types.sse_event -> unit)
    ?(metrics : Metrics.t option) () =
  let m = match metrics with Some m -> m | None -> Metrics.noop in
  let try_provider cfg =
    complete_stream ~sw ~net ?transport ~config:cfg ~messages ~tools ~on_event ()
  in
  match try_provider cascade.primary with
  | Ok _ as success -> success
  | Error err when is_retryable err ->
    let rec try_fallbacks last_err = function
      | [] -> Error last_err
      | (fb : Provider_config.t) :: rest ->
        let err_str = match last_err with
          | Http_client.HttpError { code; _ } ->
            Printf.sprintf "HTTP %d" code
          | Http_client.NetworkError { message } -> message
        in
        m.on_cascade_fallback
          ~from_model:cascade.primary.model_id ~to_model:fb.model_id
          ~reason:err_str;
        match try_provider fb with
        | Ok _ as success -> success
        | Error err2 when is_retryable err2 ->
          try_fallbacks err2 rest
        | Error _ as fail -> fail
    in
    try_fallbacks err cascade.fallbacks
  | Error _ as fail -> fail

[@@@coverage off]
(* === Inline tests === *)

let%test "is_retryable 429 rate limit" =
  is_retryable (Http_client.HttpError { code = 429; body = "" }) = true

let%test "is_retryable 500 server error" =
  is_retryable (Http_client.HttpError { code = 500; body = "" }) = true

let%test "is_retryable 502 bad gateway" =
  is_retryable (Http_client.HttpError { code = 502; body = "" }) = true

let%test "is_retryable 503 service unavailable" =
  is_retryable (Http_client.HttpError { code = 503; body = "" }) = true

let%test "is_retryable 529 overloaded" =
  is_retryable (Http_client.HttpError { code = 529; body = "" }) = true

let%test "is_retryable 400 not retryable" =
  is_retryable (Http_client.HttpError { code = 400; body = "" }) = false

let%test "is_retryable 401 not retryable" =
  is_retryable (Http_client.HttpError { code = 401; body = "" }) = false

let%test "is_retryable 404 not retryable" =
  is_retryable (Http_client.HttpError { code = 404; body = "" }) = false

let%test "is_retryable network error always retryable" =
  is_retryable (Http_client.NetworkError { message = "connection refused" }) = true

let%test "default_retry_config values" =
  default_retry_config.max_retries = 3
  && default_retry_config.initial_delay_sec = 1.0
  && default_retry_config.max_delay_sec = 30.0
  && default_retry_config.backoff_multiplier = 2.0

(* --- gemini_url tests --- *)

let%test "gemini_url sync no api_key" =
  let config : Provider_config.t = {
    kind = Provider_config.Gemini;
    model_id = "gemini-2.5-flash";
    base_url = "https://gen.googleapis.com/v1beta";
    api_key = ""; request_path = ""; headers = [];
    system_prompt = None; temperature = None; max_tokens = 1024;
    top_p = None; top_k = None; min_p = None;
    enable_thinking = None; thinking_budget = None;
    tool_choice = None; disable_parallel_tool_use = false;
    response_format_json = false; cache_system_prompt = false;
  } in
  let url = gemini_url ~config ~stream:false in
  url = "https://gen.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"

let%test "gemini_url sync with api_key" =
  let config : Provider_config.t = {
    kind = Gemini; model_id = "gemini-2.5-flash";
    base_url = "https://gen.googleapis.com/v1beta";
    api_key = "mykey"; request_path = ""; headers = [];
    system_prompt = None; temperature = None; max_tokens = 1024;
    top_p = None; top_k = None; min_p = None;
    enable_thinking = None; thinking_budget = None;
    tool_choice = None; disable_parallel_tool_use = false;
    response_format_json = false; cache_system_prompt = false;
  } in
  let url = gemini_url ~config ~stream:false in
  url = "https://gen.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key=mykey"

let%test "gemini_url stream with api_key" =
  let config : Provider_config.t = {
    kind = Gemini; model_id = "gemini-2.5-flash";
    base_url = "https://gen.googleapis.com/v1beta";
    api_key = "mykey"; request_path = ""; headers = [];
    system_prompt = None; temperature = None; max_tokens = 1024;
    top_p = None; top_k = None; min_p = None;
    enable_thinking = None; thinking_budget = None;
    tool_choice = None; disable_parallel_tool_use = false;
    response_format_json = false; cache_system_prompt = false;
  } in
  let url = gemini_url ~config ~stream:true in
  url = "https://gen.googleapis.com/v1beta/models/gemini-2.5-flash:streamGenerateContent?key=mykey&alt=sse"

let%test "gemini_url stream no api_key" =
  let config : Provider_config.t = {
    kind = Gemini; model_id = "gemini-2.5-flash";
    base_url = "https://gen.googleapis.com/v1beta";
    api_key = ""; request_path = ""; headers = [];
    system_prompt = None; temperature = None; max_tokens = 1024;
    top_p = None; top_k = None; min_p = None;
    enable_thinking = None; thinking_budget = None;
    tool_choice = None; disable_parallel_tool_use = false;
    response_format_json = false; cache_system_prompt = false;
  } in
  let url = gemini_url ~config ~stream:true in
  url = "https://gen.googleapis.com/v1beta/models/gemini-2.5-flash:streamGenerateContent?alt=sse"

let%test "is_retryable 200 not retryable" =
  is_retryable (Http_client.HttpError { code = 200; body = "" }) = false

let%test "is_retryable 403 not retryable" =
  is_retryable (Http_client.HttpError { code = 403; body = "" }) = false

(* --- provider_sampling_defaults tests --- *)

let%test "provider_sampling_defaults OpenAI_compat has min_p 0.05" =
  let d = provider_sampling_defaults Provider_config.OpenAI_compat in
  d.default_min_p = Some 0.05

let%test "provider_sampling_defaults Anthropic has no min_p" =
  let d = provider_sampling_defaults Provider_config.Anthropic in
  d.default_min_p = None

let%test "provider_sampling_defaults Gemini has no min_p" =
  let d = provider_sampling_defaults Provider_config.Gemini in
  d.default_min_p = None

let%test "provider_sampling_defaults Claude_code has no min_p" =
  let d = provider_sampling_defaults Provider_config.Claude_code in
  d.default_min_p = None

let%test "apply_sampling_defaults fills min_p for OpenAI_compat" =
  let config = Provider_config.make
    ~kind:OpenAI_compat ~model_id:"test" ~base_url:"http://localhost" () in
  let applied = apply_sampling_defaults config in
  applied.min_p = Some 0.05

let%test "apply_sampling_defaults preserves explicit min_p override" =
  let config = Provider_config.make
    ~kind:OpenAI_compat ~model_id:"test" ~base_url:"http://localhost"
    ~min_p:0.1 () in
  let applied = apply_sampling_defaults config in
  applied.min_p = Some 0.1

let%test "apply_sampling_defaults Anthropic does not set min_p" =
  let config = Provider_config.make
    ~kind:Anthropic ~model_id:"claude" ~base_url:"https://api.anthropic.com" () in
  let applied = apply_sampling_defaults config in
  applied.min_p = None

let%test "apply_sampling_defaults preserves all explicit values" =
  let config = Provider_config.make
    ~kind:OpenAI_compat ~model_id:"test" ~base_url:"http://localhost"
    ~min_p:0.2 ~top_p:0.9 ~top_k:40 () in
  let applied = apply_sampling_defaults config in
  applied.min_p = Some 0.2
  && applied.top_p = Some 0.9
  && applied.top_k = Some 40

let%test "apply_sampling_defaults Anthropic preserves explicit top_p" =
  let config = Provider_config.make
    ~kind:Anthropic ~model_id:"claude" ~base_url:"https://api.anthropic.com"
    ~top_p:0.95 () in
  let applied = apply_sampling_defaults config in
  applied.top_p = Some 0.95
