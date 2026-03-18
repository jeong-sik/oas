(** Standalone LLM completion: build -> HTTP -> parse.

    Self-contained in llm_provider -- no agent_sdk dependency.
    Both OAS and MASC can call these functions directly.

    @since 0.46.0  Sync completion
    @since 0.53.0  Streaming, retry, cascade *)

(* ── Sync completion ─────────────────────────────────────── *)

let complete ~sw ~net ~(config : Provider_config.t)
    ~(messages : Types.message list) ?(tools=[]) () =
  let body_str = match config.kind with
    | Provider_config.Anthropic ->
        Backend_anthropic.build_request ~config ~messages ~tools ()
    | Provider_config.OpenAI_compat ->
        Backend_openai.build_request ~config ~messages ~tools ()
  in
  let url = config.base_url ^ config.request_path in
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
        in
        Ok response
      else
        Error (Http_client.HttpError { code; body })

(* ── Retry ───────────────────────────────────────────────── *)

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

let complete_with_retry ~sw ~net ~clock
    ~(config : Provider_config.t)
    ~(messages : Types.message list) ?(tools=[])
    ?(retry_config=default_retry_config) () =
  let rec attempt n delay =
    match complete ~sw ~net ~config ~messages ~tools () with
    | Ok _ as success -> success
    | Error err when is_retryable err && n < retry_config.max_retries ->
        Eio.Time.sleep clock delay;
        let next_delay =
          Float.min
            (delay *. retry_config.backoff_multiplier)
            retry_config.max_delay_sec
        in
        attempt (n + 1) next_delay
    | Error _ as fail -> fail
  in
  attempt 0 retry_config.initial_delay_sec

(* ── Cascade: multi-provider failover ────────────────────── *)

type cascade = {
  primary: Provider_config.t;
  fallbacks: Provider_config.t list;
}

let complete_cascade ~sw ~net ?clock ?retry_config
    ~(cascade : cascade)
    ~(messages : Types.message list) ?(tools=[]) () =
  let try_provider cfg =
    match clock with
    | Some clock ->
        complete_with_retry ~sw ~net ~clock ~config:cfg
          ~messages ~tools ?retry_config ()
    | None ->
        complete ~sw ~net ~config:cfg ~messages ~tools ()
  in
  match try_provider cascade.primary with
  | Ok _ as success -> success
  | Error err ->
      if is_retryable err then
        let rec try_fallbacks last_err = function
          | [] -> Error last_err
          | fb :: rest ->
              match try_provider fb with
              | Ok _ as success -> success
              | Error err2 ->
                  if is_retryable err2 then try_fallbacks err2 rest
                  else Error err2
        in
        try_fallbacks err cascade.fallbacks
      else Error err

(* ── Streaming ───────────────────────────────────────────── *)

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
          with _ -> `Assoc [] in
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

let complete_stream ~sw ~net ~(config : Provider_config.t)
    ~(messages : Types.message list) ?(tools=[])
    ~(on_event : Types.sse_event -> unit) () =
  let body_str = match config.kind with
    | Provider_config.Anthropic ->
        Backend_anthropic.build_request ~stream:true ~config ~messages ~tools ()
    | Provider_config.OpenAI_compat ->
        Backend_openai.build_request ~stream:true ~config ~messages ~tools ()
  in
  let url = config.base_url ^ config.request_path in
  let body_with_stream = Http_client.inject_stream_param body_str in
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
        in
        List.iter (fun evt ->
          on_event evt;
          accumulate_event acc evt
        ) events
      ) ();
      Ok (finalize_stream_acc acc)
