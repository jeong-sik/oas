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

let complete_http ~sw ~net ~(config : Provider_config.t)
    ~(messages : Types.message list) ~tools =
  if config.kind = Provider_config.Claude_code then
    (Error (Http_client.NetworkError {
       message = "Claude_code provider requires a transport (use Transport_claude_code.create)" }),
     0)
  else
  let body_str = match config.kind with
    | Provider_config.Anthropic ->
        Backend_anthropic.build_request ~config ~messages ~tools ()
    | Provider_config.OpenAI_compat ->
        Backend_openai.build_request ~config ~messages ~tools ()
    | Provider_config.Gemini ->
        Backend_gemini.build_request ~config ~messages ~tools ()
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
  let cached = match cache with
    | Some c ->
        let key = Cache.request_fingerprint ~config ~messages ~tools () in
        (match c.get ~key with
         | Some json ->
             (match Cache.response_of_json json with
              | Some resp ->
                  m.on_cache_hit ~model_id;
                  Some (Ok resp, key)
              | None ->
                  m.on_cache_miss ~model_id;
                  None)
         | None ->
             m.on_cache_miss ~model_id;
             None)
    | None -> None
  in
  match cached with
  | Some (result, _key) -> result
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
           (* Cache store *)
           (match cache with
            | Some c ->
                let key = Cache.request_fingerprint ~config ~messages ~tools () in
                let json = Cache.response_to_json resp in
                (try c.set ~key ~ttl_sec:300 json
                 with Eio.Io _ | Sys_error _ -> ())
            | None -> ());
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

(** Internal: accumulate SSE events into content blocks. *)
type stream_acc = {
  id: string ref;
  model: string ref;
  input_tokens: int ref;
  output_tokens: int ref;
  cache_creation: int ref;
  cache_read: int ref;
  stop_reason: Types.stop_reason ref;
  block_texts: (int, Buffer.t) Hashtbl.t;
  block_types: (int, string) Hashtbl.t;
  block_tool_ids: (int, string) Hashtbl.t;
  block_tool_names: (int, string) Hashtbl.t;
}

let create_stream_acc () = {
  id = ref ""; model = ref "";
  input_tokens = ref 0; output_tokens = ref 0;
  cache_creation = ref 0; cache_read = ref 0;
  stop_reason = ref Types.EndTurn;
  block_texts = Hashtbl.create 4;
  block_types = Hashtbl.create 4;
  block_tool_ids = Hashtbl.create 4;
  block_tool_names = Hashtbl.create 4;
}

let accumulate_event (acc : stream_acc) = function
  | Types.MessageStart { id; model; usage } ->
      acc.id := id; acc.model := model;
      (match usage with
       | Some u ->
           acc.input_tokens := u.input_tokens;
           acc.cache_creation := u.cache_creation_input_tokens;
           acc.cache_read := u.cache_read_input_tokens
       | None -> ())
  | Types.ContentBlockStart { index; content_type; tool_id; tool_name } ->
      Hashtbl.replace acc.block_types index content_type;
      Hashtbl.replace acc.block_texts index (Buffer.create 64);
      (match tool_id with
       | Some id -> Hashtbl.replace acc.block_tool_ids index id | None -> ());
      (match tool_name with
       | Some n -> Hashtbl.replace acc.block_tool_names index n | None -> ())
  | Types.ContentBlockDelta { index; delta } ->
      let buf = match Hashtbl.find_opt acc.block_texts index with
        | Some b -> b
        | None ->
            let b = Buffer.create 64 in
            Hashtbl.replace acc.block_texts index b; b
      in
      (match delta with
       | Types.TextDelta s | Types.ThinkingDelta s | Types.InputJsonDelta s ->
           Buffer.add_string buf s)
  | Types.ContentBlockStop _ -> ()
  | Types.MessageDelta { stop_reason; usage } ->
      (match stop_reason with Some sr -> acc.stop_reason := sr | None -> ());
      (match usage with
       | Some u -> acc.output_tokens := !(acc.output_tokens) + u.output_tokens
       | None -> ())
  | Types.MessageStop | Types.Ping | Types.SSEError _ -> ()

let finalize_stream_acc (acc : stream_acc) =
  let indices =
    Hashtbl.fold (fun k _ acc -> k :: acc) acc.block_types []
    |> List.sort compare
  in
  let content = List.filter_map (fun idx ->
    let text = match Hashtbl.find_opt acc.block_texts idx with
      | Some buf -> Buffer.contents buf | None -> ""
    in
    match Hashtbl.find_opt acc.block_types idx with
    | Some "text" -> Some (Types.Text text)
    | Some "thinking" ->
        Some (Types.Thinking { thinking_type = "thinking"; content = text })
    | Some "tool_use" ->
        let id = match Hashtbl.find_opt acc.block_tool_ids idx with
          | Some s -> s | None -> "" in
        let name = match Hashtbl.find_opt acc.block_tool_names idx with
          | Some s -> s | None -> "" in
        let input = try Yojson.Safe.from_string text
          with Yojson.Json_error _ -> `Assoc [] in
        Some (Types.ToolUse { id; name; input })
    | _ -> None
  ) indices in
  { Types.id = !(acc.id);
    model = !(acc.model);
    stop_reason = !(acc.stop_reason);
    content;
    usage = Some {
      input_tokens = !(acc.input_tokens);
      output_tokens = !(acc.output_tokens);
      cache_creation_input_tokens = !(acc.cache_creation);
      cache_read_input_tokens = !(acc.cache_read);
    } }

(* Internal: HTTP-specific streaming implementation. *)
let complete_stream_http ~sw ~net ~(config : Provider_config.t)
    ~(messages : Types.message list) ~tools
    ~(on_event : Types.sse_event -> unit) =
  if config.kind = Provider_config.Claude_code then
    Error (Http_client.NetworkError {
      message = "Claude_code provider requires a transport (use Transport_claude_code.create)" })
  else
  let body_str = match config.kind with
    | Provider_config.Anthropic ->
        Backend_anthropic.build_request ~stream:true ~config ~messages ~tools ()
    | Provider_config.OpenAI_compat ->
        Backend_openai.build_request ~stream:true ~config ~messages ~tools ()
    | Provider_config.Gemini ->
        Backend_gemini.build_request ~stream:true ~config ~messages ~tools ()
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
          | Provider_config.Claude_code -> []  (* guarded above *)
        in
        List.iter (fun evt ->
          on_event evt;
          accumulate_event acc evt
        ) events
      ) ();
      Ok (finalize_stream_acc acc)

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

let%test "create_stream_acc has sensible defaults" =
  let acc = create_stream_acc () in
  !(acc.id) = ""
  && !(acc.model) = ""
  && !(acc.input_tokens) = 0
  && !(acc.output_tokens) = 0
  && !(acc.stop_reason) = Types.EndTurn

let%test "accumulate_event MessageStart sets id and model" =
  let acc = create_stream_acc () in
  accumulate_event acc (Types.MessageStart {
    id = "msg-1"; model = "gpt-4"; usage = None });
  !(acc.id) = "msg-1" && !(acc.model) = "gpt-4"

let%test "accumulate_event MessageStart with usage" =
  let acc = create_stream_acc () in
  accumulate_event acc (Types.MessageStart {
    id = "msg-2"; model = "m";
    usage = Some { input_tokens = 100; output_tokens = 0;
                   cache_creation_input_tokens = 5; cache_read_input_tokens = 10 }});
  !(acc.input_tokens) = 100
  && !(acc.cache_creation) = 5
  && !(acc.cache_read) = 10

let%test "accumulate_event ContentBlockStart + Delta + Stop" =
  let acc = create_stream_acc () in
  accumulate_event acc (Types.ContentBlockStart {
    index = 0; content_type = "text"; tool_id = None; tool_name = None });
  accumulate_event acc (Types.ContentBlockDelta {
    index = 0; delta = Types.TextDelta "Hello " });
  accumulate_event acc (Types.ContentBlockDelta {
    index = 0; delta = Types.TextDelta "world" });
  accumulate_event acc (Types.ContentBlockStop { index = 0 });
  let buf = Hashtbl.find acc.block_texts 0 in
  Buffer.contents buf = "Hello world"

let%test "accumulate_event MessageDelta sets stop_reason" =
  let acc = create_stream_acc () in
  accumulate_event acc (Types.MessageDelta {
    stop_reason = Some Types.StopToolUse;
    usage = Some { input_tokens = 0; output_tokens = 50;
                   cache_creation_input_tokens = 0; cache_read_input_tokens = 0 }});
  !(acc.stop_reason) = Types.StopToolUse
  && !(acc.output_tokens) = 50

let%test "finalize_stream_acc assembles text block" =
  let acc = create_stream_acc () in
  acc.id := "test-id";
  acc.model := "test-model";
  Hashtbl.replace acc.block_types 0 "text";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "Hello world";
  Hashtbl.replace acc.block_texts 0 buf;
  let result = finalize_stream_acc acc in
  result.id = "test-id"
  && result.model = "test-model"
  && result.content = [Types.Text "Hello world"]

let%test "finalize_stream_acc assembles tool_use block" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "tool_use";
  Hashtbl.replace acc.block_tool_ids 0 "tool-id-1";
  Hashtbl.replace acc.block_tool_names 0 "my_tool";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "{\"key\":\"val\"}";
  Hashtbl.replace acc.block_texts 0 buf;
  let result = finalize_stream_acc acc in
  match result.content with
  | [Types.ToolUse { id = "tool-id-1"; name = "my_tool"; input }] ->
    input = `Assoc [("key", `String "val")]
  | _ -> false

let%test "finalize_stream_acc assembles thinking block" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "thinking";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "reasoning...";
  Hashtbl.replace acc.block_texts 0 buf;
  let result = finalize_stream_acc acc in
  match result.content with
  | [Types.Thinking { thinking_type = "thinking"; content = "reasoning..." }] -> true
  | _ -> false

let%test "finalize_stream_acc multiple blocks ordered by index" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "thinking";
  Hashtbl.replace acc.block_types 1 "text";
  let buf0 = Buffer.create 16 in Buffer.add_string buf0 "think";
  let buf1 = Buffer.create 16 in Buffer.add_string buf1 "say";
  Hashtbl.replace acc.block_texts 0 buf0;
  Hashtbl.replace acc.block_texts 1 buf1;
  let result = finalize_stream_acc acc in
  List.length result.content = 2

let%test "finalize_stream_acc includes usage" =
  let acc = create_stream_acc () in
  acc.input_tokens := 100;
  acc.output_tokens := 50;
  acc.cache_creation := 10;
  acc.cache_read := 20;
  let result = finalize_stream_acc acc in
  match result.usage with
  | Some u ->
    u.input_tokens = 100 && u.output_tokens = 50
    && u.cache_creation_input_tokens = 10 && u.cache_read_input_tokens = 20
  | None -> false

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

(* --- accumulate_event edge cases --- *)

let%test "accumulate_event ContentBlockDelta on unknown index creates buffer" =
  let acc = create_stream_acc () in
  accumulate_event acc (Types.ContentBlockDelta {
    index = 99; delta = Types.TextDelta "orphan" });
  match Hashtbl.find_opt acc.block_texts 99 with
  | Some buf -> Buffer.contents buf = "orphan"
  | None -> false

let%test "accumulate_event ContentBlockStart with tool_id and tool_name" =
  let acc = create_stream_acc () in
  accumulate_event acc (Types.ContentBlockStart {
    index = 0; content_type = "tool_use";
    tool_id = Some "tid-1"; tool_name = Some "my_fn" });
  Hashtbl.find acc.block_tool_ids 0 = "tid-1"
  && Hashtbl.find acc.block_tool_names 0 = "my_fn"

let%test "accumulate_event ThinkingDelta appends to buffer" =
  let acc = create_stream_acc () in
  accumulate_event acc (Types.ContentBlockStart {
    index = 0; content_type = "thinking"; tool_id = None; tool_name = None });
  accumulate_event acc (Types.ContentBlockDelta {
    index = 0; delta = Types.ThinkingDelta "step1" });
  accumulate_event acc (Types.ContentBlockDelta {
    index = 0; delta = Types.ThinkingDelta " step2" });
  let buf = Hashtbl.find acc.block_texts 0 in
  Buffer.contents buf = "step1 step2"

let%test "accumulate_event InputJsonDelta appends to buffer" =
  let acc = create_stream_acc () in
  accumulate_event acc (Types.ContentBlockStart {
    index = 0; content_type = "tool_use"; tool_id = None; tool_name = None });
  accumulate_event acc (Types.ContentBlockDelta {
    index = 0; delta = Types.InputJsonDelta "{\"k\":" });
  accumulate_event acc (Types.ContentBlockDelta {
    index = 0; delta = Types.InputJsonDelta "\"v\"}" });
  let buf = Hashtbl.find acc.block_texts 0 in
  Buffer.contents buf = "{\"k\":\"v\"}"

let%test "accumulate_event MessageDelta None stop_reason keeps default" =
  let acc = create_stream_acc () in
  accumulate_event acc (Types.MessageDelta {
    stop_reason = None; usage = None });
  !(acc.stop_reason) = Types.EndTurn

let%test "accumulate_event MessageDelta None usage does not change tokens" =
  let acc = create_stream_acc () in
  acc.output_tokens := 10;
  accumulate_event acc (Types.MessageDelta {
    stop_reason = None; usage = None });
  !(acc.output_tokens) = 10

let%test "accumulate_event MessageStop is no-op" =
  let acc = create_stream_acc () in
  acc.id := "keep";
  accumulate_event acc Types.MessageStop;
  !(acc.id) = "keep"

let%test "accumulate_event Ping is no-op" =
  let acc = create_stream_acc () in
  accumulate_event acc Types.Ping;
  !(acc.id) = ""

let%test "accumulate_event SSEError is no-op" =
  let acc = create_stream_acc () in
  accumulate_event acc (Types.SSEError "bad");
  !(acc.id) = ""

(* --- finalize_stream_acc edge cases --- *)

let%test "finalize_stream_acc empty produces empty content" =
  let acc = create_stream_acc () in
  let result = finalize_stream_acc acc in
  result.content = []

let%test "finalize_stream_acc unknown block type filtered out" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "unknown_type";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "data";
  Hashtbl.replace acc.block_texts 0 buf;
  let result = finalize_stream_acc acc in
  result.content = []

let%test "finalize_stream_acc tool_use with invalid json falls back to empty assoc" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "tool_use";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "not valid json";
  Hashtbl.replace acc.block_texts 0 buf;
  let result = finalize_stream_acc acc in
  match result.content with
  | [Types.ToolUse { input = `Assoc []; _ }] -> true
  | _ -> false

let%test "finalize_stream_acc tool_use missing id/name defaults to empty" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "tool_use";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "{}";
  Hashtbl.replace acc.block_texts 0 buf;
  let result = finalize_stream_acc acc in
  match result.content with
  | [Types.ToolUse { id = ""; name = ""; _ }] -> true
  | _ -> false

let%test "finalize_stream_acc block with no text buffer produces empty text" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "text";
  (* No buffer added for index 0 *)
  let result = finalize_stream_acc acc in
  match result.content with
  | [Types.Text ""] -> true
  | _ -> false

let%test "is_retryable 200 not retryable" =
  is_retryable (Http_client.HttpError { code = 200; body = "" }) = false

let%test "is_retryable 403 not retryable" =
  is_retryable (Http_client.HttpError { code = 403; body = "" }) = false

let%test "accumulate_event multiple MessageDelta accumulates tokens" =
  let acc = create_stream_acc () in
  accumulate_event acc (Types.MessageDelta {
    stop_reason = None;
    usage = Some { input_tokens = 0; output_tokens = 30;
                   cache_creation_input_tokens = 0; cache_read_input_tokens = 0 }});
  accumulate_event acc (Types.MessageDelta {
    stop_reason = None;
    usage = Some { input_tokens = 0; output_tokens = 20;
                   cache_creation_input_tokens = 0; cache_read_input_tokens = 0 }});
  !(acc.output_tokens) = 50
