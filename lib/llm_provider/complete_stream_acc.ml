(** Stream accumulator: gather SSE events into a {!Types.api_response}.

    Extracts the stream_acc type and its operations from [Complete].
    Depends only on [Types] -- no provider/backend/transport references.

    @since 0.79.0 *)

(** Internal: accumulate SSE events into content blocks. *)
type stream_acc =
  { id : string ref
  ; model : string ref
  ; input_tokens : int ref
  ; output_tokens : int ref
  ; cache_creation : int ref
  ; cache_read : int ref
  ; stop_reason : Types.stop_reason ref
  ; sse_error : string option ref
  ; block_texts : (int, Buffer.t) Hashtbl.t
  ; block_types : (int, string) Hashtbl.t
  ; block_tool_ids : (int, string) Hashtbl.t
  ; block_tool_names : (int, string) Hashtbl.t
  }

let create_stream_acc () =
  { id = ref ""
  ; model = ref ""
  ; input_tokens = ref 0
  ; output_tokens = ref 0
  ; cache_creation = ref 0
  ; cache_read = ref 0
  ; stop_reason = ref Types.EndTurn
  ; sse_error = ref None
  ; block_texts = Hashtbl.create 4
  ; block_types = Hashtbl.create 4
  ; block_tool_ids = Hashtbl.create 4
  ; block_tool_names = Hashtbl.create 4
  }
;;

let accumulate_event (acc : stream_acc) = function
  | Types.MessageStart { id; model; usage } ->
    acc.id := id;
    acc.model := model;
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
     | Some id -> Hashtbl.replace acc.block_tool_ids index id
     | None -> ());
    (match tool_name with
     | Some n -> Hashtbl.replace acc.block_tool_names index n
     | None -> ())
  | Types.ContentBlockDelta { index; delta } ->
    let buf =
      match Hashtbl.find_opt acc.block_texts index with
      | Some b -> b
      | None ->
        let b = Buffer.create 64 in
        Hashtbl.replace acc.block_texts index b;
        b
    in
    (match delta with
     | Types.TextDelta s | Types.ThinkingDelta s | Types.InputJsonDelta s ->
       Buffer.add_string buf s)
  | Types.ContentBlockStop _ -> ()
  | Types.MessageDelta { stop_reason; usage } ->
    (match stop_reason with
     | Some sr -> acc.stop_reason := sr
     | None -> ());
    (match usage with
     | Some u -> acc.output_tokens := !(acc.output_tokens) + u.output_tokens
     | None -> ())
  | Types.SSEError msg -> acc.sse_error := Some msg
  | Types.SSEParseFailed { raw; reason } ->
    let preview =
      if String.length raw > 200 then String.sub raw 0 200 ^ "...(truncated)" else raw
    in
    acc.sse_error
    := Some (Printf.sprintf "sse_parse_failed: %s | chunk: %s" reason preview)
  | Types.SSEUnknownEventType { event_type; raw } ->
    let preview =
      if String.length raw > 200 then String.sub raw 0 200 ^ "...(truncated)" else raw
    in
    acc.sse_error
    := Some (Printf.sprintf "sse_unknown_event_type: %s | chunk: %s" event_type preview)
  | Types.MessageStop | Types.Ping -> ()
;;

let finalize_stream_acc (acc : stream_acc) =
  match !(acc.sse_error) with
  | Some msg -> Error msg
  | None ->
    let indices =
      Hashtbl.fold (fun k _ acc -> k :: acc) acc.block_types [] |> List.sort compare
    in
    let content =
      List.filter_map
        (fun idx ->
           let text =
             match Hashtbl.find_opt acc.block_texts idx with
             | Some buf -> Buffer.contents buf
             | None -> ""
           in
           match Hashtbl.find_opt acc.block_types idx with
           | Some "text" -> Some (Types.Text text)
           | Some "thinking" ->
             Some (Types.Thinking { thinking_type = "thinking"; content = text })
           | Some "tool_use" ->
             let id =
               match Hashtbl.find_opt acc.block_tool_ids idx with
               | Some s -> s
               | None -> ""
             in
             let name =
               match Hashtbl.find_opt acc.block_tool_names idx with
               | Some s -> s
               | None -> ""
             in
             let input =
               try Yojson.Safe.from_string text with
               | Yojson.Json_error _ -> `Assoc []
             in
             Some (Types.ToolUse { id; name; input })
           | Some "tool_result" | Some "tool_result_error" ->
             let tool_use_id =
               match Hashtbl.find_opt acc.block_tool_ids idx with
               | Some s -> s
               | None -> ""
             in
             let is_error =
               match Hashtbl.find_opt acc.block_types idx with
               | Some "tool_result_error" -> true
               | _ -> false
             in
             Some
               (Types.ToolResult
                  { tool_use_id
                  ; content = text
                  ; is_error
                  ; json = (if is_error then None else Types.try_parse_json text)
                  })
           | _ -> None)
        indices
    in
    Ok
      { Types.id = !(acc.id)
      ; model = !(acc.model)
      ; stop_reason = !(acc.stop_reason)
      ; content
      ; usage =
          Some
            { input_tokens = !(acc.input_tokens)
            ; output_tokens = !(acc.output_tokens)
            ; cache_creation_input_tokens = !(acc.cache_creation)
            ; cache_read_input_tokens = !(acc.cache_read)
            ; cost_usd = None
            }
      ; telemetry = None
      }
;;

[@@@coverage off]
(* === Inline tests === *)

let%test "create_stream_acc has sensible defaults" =
  let acc = create_stream_acc () in
  !(acc.id) = ""
  && !(acc.model) = ""
  && !(acc.input_tokens) = 0
  && !(acc.output_tokens) = 0
  && !(acc.stop_reason) = Types.EndTurn
  && !(acc.sse_error) = None
;;

let%test "accumulate_event MessageStart sets id and model" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.MessageStart { id = "msg-1"; model = "gpt-4"; usage = None });
  !(acc.id) = "msg-1" && !(acc.model) = "gpt-4"
;;

let%test "accumulate_event MessageStart with usage" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.MessageStart
       { id = "msg-2"
       ; model = "m"
       ; usage =
           Some
             { input_tokens = 100
             ; output_tokens = 0
             ; cache_creation_input_tokens = 5
             ; cache_read_input_tokens = 10
             ; cost_usd = None
             }
       });
  !(acc.input_tokens) = 100 && !(acc.cache_creation) = 5 && !(acc.cache_read) = 10
;;

let%test "accumulate_event ContentBlockStart + Delta + Stop" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.ContentBlockStart
       { index = 0; content_type = "text"; tool_id = None; tool_name = None });
  accumulate_event
    acc
    (Types.ContentBlockDelta { index = 0; delta = Types.TextDelta "Hello " });
  accumulate_event
    acc
    (Types.ContentBlockDelta { index = 0; delta = Types.TextDelta "world" });
  accumulate_event acc (Types.ContentBlockStop { index = 0 });
  let buf = Hashtbl.find acc.block_texts 0 in
  Buffer.contents buf = "Hello world"
;;

let%test "accumulate_event MessageDelta sets stop_reason" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.MessageDelta
       { stop_reason = Some Types.StopToolUse
       ; usage =
           Some
             { input_tokens = 0
             ; output_tokens = 50
             ; cache_creation_input_tokens = 0
             ; cache_read_input_tokens = 0
             ; cost_usd = None
             }
       });
  !(acc.stop_reason) = Types.StopToolUse && !(acc.output_tokens) = 50
;;

let%test "finalize_stream_acc assembles text block" =
  let acc = create_stream_acc () in
  acc.id := "test-id";
  acc.model := "test-model";
  Hashtbl.replace acc.block_types 0 "text";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "Hello world";
  Hashtbl.replace acc.block_texts 0 buf;
  match finalize_stream_acc acc with
  | Error _ -> false
  | Ok result ->
    result.id = "test-id"
    && result.model = "test-model"
    && result.content = [ Types.Text "Hello world" ]
;;

let%test "finalize_stream_acc assembles tool_use block" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "tool_use";
  Hashtbl.replace acc.block_tool_ids 0 "tool-id-1";
  Hashtbl.replace acc.block_tool_names 0 "my_tool";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "{\"key\":\"val\"}";
  Hashtbl.replace acc.block_texts 0 buf;
  match finalize_stream_acc acc with
  | Error _ -> false
  | Ok result ->
    (match result.content with
     | [ Types.ToolUse { id = "tool-id-1"; name = "my_tool"; input } ] ->
       input = `Assoc [ "key", `String "val" ]
     | _ -> false)
;;

let%test "finalize_stream_acc assembles thinking block" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "thinking";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "reasoning...";
  Hashtbl.replace acc.block_texts 0 buf;
  match finalize_stream_acc acc with
  | Error _ -> false
  | Ok result ->
    (match result.content with
     | [ Types.Thinking { thinking_type = "thinking"; content = "reasoning..." } ] -> true
     | _ -> false)
;;

let%test "finalize_stream_acc multiple blocks ordered by index" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "thinking";
  Hashtbl.replace acc.block_types 1 "text";
  let buf0 = Buffer.create 16 in
  Buffer.add_string buf0 "think";
  let buf1 = Buffer.create 16 in
  Buffer.add_string buf1 "say";
  Hashtbl.replace acc.block_texts 0 buf0;
  Hashtbl.replace acc.block_texts 1 buf1;
  match finalize_stream_acc acc with
  | Error _ -> false
  | Ok result -> List.length result.content = 2
;;

let%test "finalize_stream_acc includes usage" =
  let acc = create_stream_acc () in
  acc.input_tokens := 100;
  acc.output_tokens := 50;
  acc.cache_creation := 10;
  acc.cache_read := 20;
  match finalize_stream_acc acc with
  | Error _ -> false
  | Ok result ->
    (match result.usage with
     | Some u ->
       u.input_tokens = 100
       && u.output_tokens = 50
       && u.cache_creation_input_tokens = 10
       && u.cache_read_input_tokens = 20
     | None -> false)
;;

(* --- accumulate_event edge cases --- *)

let%test "accumulate_event ContentBlockDelta on unknown index creates buffer" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.ContentBlockDelta { index = 99; delta = Types.TextDelta "orphan" });
  match Hashtbl.find_opt acc.block_texts 99 with
  | Some buf -> Buffer.contents buf = "orphan"
  | None -> false
;;

let%test "accumulate_event ContentBlockStart with tool_id and tool_name" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.ContentBlockStart
       { index = 0
       ; content_type = "tool_use"
       ; tool_id = Some "tid-1"
       ; tool_name = Some "my_fn"
       });
  Hashtbl.find acc.block_tool_ids 0 = "tid-1"
  && Hashtbl.find acc.block_tool_names 0 = "my_fn"
;;

let%test "accumulate_event ThinkingDelta appends to buffer" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.ContentBlockStart
       { index = 0; content_type = "thinking"; tool_id = None; tool_name = None });
  accumulate_event
    acc
    (Types.ContentBlockDelta { index = 0; delta = Types.ThinkingDelta "step1" });
  accumulate_event
    acc
    (Types.ContentBlockDelta { index = 0; delta = Types.ThinkingDelta " step2" });
  let buf = Hashtbl.find acc.block_texts 0 in
  Buffer.contents buf = "step1 step2"
;;

let%test "accumulate_event InputJsonDelta appends to buffer" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.ContentBlockStart
       { index = 0; content_type = "tool_use"; tool_id = None; tool_name = None });
  accumulate_event
    acc
    (Types.ContentBlockDelta { index = 0; delta = Types.InputJsonDelta "{\"k\":" });
  accumulate_event
    acc
    (Types.ContentBlockDelta { index = 0; delta = Types.InputJsonDelta "\"v\"}" });
  let buf = Hashtbl.find acc.block_texts 0 in
  Buffer.contents buf = "{\"k\":\"v\"}"
;;

let%test "accumulate_event MessageDelta None stop_reason keeps default" =
  let acc = create_stream_acc () in
  accumulate_event acc (Types.MessageDelta { stop_reason = None; usage = None });
  !(acc.stop_reason) = Types.EndTurn
;;

let%test "accumulate_event MessageDelta None usage does not change tokens" =
  let acc = create_stream_acc () in
  acc.output_tokens := 10;
  accumulate_event acc (Types.MessageDelta { stop_reason = None; usage = None });
  !(acc.output_tokens) = 10
;;

let%test "accumulate_event MessageStop is no-op" =
  let acc = create_stream_acc () in
  acc.id := "keep";
  accumulate_event acc Types.MessageStop;
  !(acc.id) = "keep"
;;

let%test "accumulate_event Ping is no-op" =
  let acc = create_stream_acc () in
  accumulate_event acc Types.Ping;
  !(acc.id) = ""
;;

let%test "accumulate_event SSEError records error" =
  let acc = create_stream_acc () in
  accumulate_event acc (Types.SSEError "bad");
  !(acc.sse_error) = Some "bad"
;;

(* SSEParseFailed and SSEUnknownEventType replace the previous silent [None]
   discard in [parse_sse_event]. They MUST mark [sse_error] so that
   [finalize_stream_acc] yields [Error _] and the cascade layer can route to
   another provider instead of presenting a phantom completion (a partial
   response with no MessageStop, treated as success by downstream consumers). *)

let%test "accumulate_event SSEParseFailed marks stream error with reason" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.SSEParseFailed { raw = "{not json"; reason = "json_error: Line 1, bytes 0-9" });
  match !(acc.sse_error) with
  | Some msg ->
    let starts s prefix =
      String.length s >= String.length prefix
      && String.sub s 0 (String.length prefix) = prefix
    in
    starts msg "sse_parse_failed:"
  | None -> false
;;

let%test "accumulate_event SSEUnknownEventType marks stream error with type" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.SSEUnknownEventType
       { event_type = "future_event_v3"; raw = "{\"type\":\"future_event_v3\"}" });
  match !(acc.sse_error) with
  | Some msg ->
    let contains s sub =
      let n = String.length s in
      let m = String.length sub in
      let rec loop i = i + m <= n && (String.sub s i m = sub || loop (i + 1)) in
      m <= n && loop 0
    in
    contains msg "future_event_v3"
  | None -> false
;;

let%test "accumulate_event SSEParseFailed truncates large raw chunks" =
  let acc = create_stream_acc () in
  let big = String.make 5000 'x' in
  accumulate_event acc (Types.SSEParseFailed { raw = big; reason = "test" });
  match !(acc.sse_error) with
  | Some msg ->
    let contains s sub =
      let n = String.length s in
      let m = String.length sub in
      let rec loop i = i + m <= n && (String.sub s i m = sub || loop (i + 1)) in
      m <= n && loop 0
    in
    String.length msg < 5000 && contains msg "truncated"
  | None -> false
;;

(* --- finalize_stream_acc edge cases --- *)

let%test "finalize_stream_acc empty produces empty content" =
  let acc = create_stream_acc () in
  match finalize_stream_acc acc with
  | Error _ -> false
  | Ok result -> result.content = []
;;

let%test "finalize_stream_acc unknown block type filtered out" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "unknown_type";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "data";
  Hashtbl.replace acc.block_texts 0 buf;
  match finalize_stream_acc acc with
  | Error _ -> false
  | Ok result -> result.content = []
;;

let%test "finalize_stream_acc tool_use with invalid json falls back to empty assoc" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "tool_use";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "not valid json";
  Hashtbl.replace acc.block_texts 0 buf;
  match finalize_stream_acc acc with
  | Error _ -> false
  | Ok result ->
    (match result.content with
     | [ Types.ToolUse { input = `Assoc []; _ } ] -> true
     | _ -> false)
;;

let%test "finalize_stream_acc tool_use missing id/name defaults to empty" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "tool_use";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "{}";
  Hashtbl.replace acc.block_texts 0 buf;
  match finalize_stream_acc acc with
  | Error _ -> false
  | Ok result ->
    (match result.content with
     | [ Types.ToolUse { id = ""; name = ""; _ } ] -> true
     | _ -> false)
;;

let%test "finalize_stream_acc assembles tool_result block" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "tool_result";
  Hashtbl.replace acc.block_tool_ids 0 "tu_1";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "{\"ok\":true}";
  Hashtbl.replace acc.block_texts 0 buf;
  match finalize_stream_acc acc with
  | Error _ -> false
  | Ok result ->
    (match result.content with
     | [ Types.ToolResult
           { tool_use_id = "tu_1"
           ; content = "{\"ok\":true}"
           ; json = Some (`Assoc [ ("ok", `Bool true) ])
           ; _
           }
       ] -> true
     | _ -> false)
;;

let%test "finalize_stream_acc block with no text buffer produces empty text" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "text";
  (* No buffer added for index 0 *)
  match finalize_stream_acc acc with
  | Error _ -> false
  | Ok result ->
    (match result.content with
     | [ Types.Text "" ] -> true
     | _ -> false)
;;

let%test "finalize_stream_acc returns Error when sse_error is set" =
  let acc = create_stream_acc () in
  acc.id := "partial-id";
  Hashtbl.replace acc.block_types 0 "text";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "partial content";
  Hashtbl.replace acc.block_texts 0 buf;
  acc.sse_error := Some "server overloaded";
  match finalize_stream_acc acc with
  | Error msg -> msg = "server overloaded"
  | Ok _ -> false
;;

let%test "accumulate_event multiple MessageDelta accumulates tokens" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.MessageDelta
       { stop_reason = None
       ; usage =
           Some
             { input_tokens = 0
             ; output_tokens = 30
             ; cache_creation_input_tokens = 0
             ; cache_read_input_tokens = 0
             ; cost_usd = None
             }
       });
  accumulate_event
    acc
    (Types.MessageDelta
       { stop_reason = None
       ; usage =
           Some
             { input_tokens = 0
             ; output_tokens = 20
             ; cache_creation_input_tokens = 0
             ; cache_read_input_tokens = 0
             ; cost_usd = None
             }
       });
  !(acc.output_tokens) = 50
;;
