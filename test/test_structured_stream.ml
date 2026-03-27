(** Tests for Structured output + SSE streaming integration.

    Verifies that emit_synthetic_events, parse_sse_event, and
    extract_tool_input compose correctly for the structured output
    streaming pattern (extract_stream). *)

open Agent_sdk
open Types

(* ── Schemas ─────────────────────────────────────────────────────── *)

let person_schema : (string * int) Structured.schema = {
  name = "extract_person";
  description = "Extract person info";
  params = [
    { name = "name"; description = "Person name"; param_type = String; required = true };
    { name = "age"; description = "Person age"; param_type = Integer; required = true };
  ];
  parse = (fun json ->
    let open Yojson.Safe.Util in
    try
      let name = json |> member "name" |> to_string in
      let age = json |> member "age" |> to_int in
      Ok (name, age)
    with exn -> Error (Printexc.to_string exn));
}

let color_schema : string Structured.schema = {
  name = "extract_color";
  description = "Extract color";
  params = [
    { name = "color"; description = "Color name"; param_type = String; required = true };
  ];
  parse = (fun json ->
    let open Yojson.Safe.Util in
    try Ok (json |> member "color" |> to_string)
    with exn -> Error (Printexc.to_string exn));
}

(* ── Helpers ─────────────────────────────────────────────────────── *)

let make_tool_response ~tool_id ~tool_name ~input_json =
  { id = "msg_test"; model = "claude-sonnet-4";
    stop_reason = StopToolUse;
    content = [ToolUse { id = tool_id; name = tool_name; input = input_json }];
    usage = Some { input_tokens = 50; output_tokens = 20;
                   cache_creation_input_tokens = 0; cache_read_input_tokens = 0 ; cost_usd = None } }

(** Build a proper SSE data JSON for an InputJsonDelta event.
    Uses Yojson to handle escaping correctly. *)
let make_delta_data index partial_json =
  Yojson.Safe.to_string (`Assoc [
    ("type", `String "content_block_delta");
    ("index", `Int index);
    ("delta", `Assoc [
      ("type", `String "input_json_delta");
      ("partial_json", `String partial_json);
    ]);
  ])

(* ── 1. synthetic_events_for_tool_use ────────────────────────────── *)

let test_synthetic_events_for_tool_use () =
  let input_json = `Assoc [("name", `String "Bob"); ("age", `Int 25)] in
  let response = make_tool_response ~tool_id:"tu_1"
    ~tool_name:"extract_person" ~input_json in
  let events = ref [] in
  Streaming.emit_synthetic_events response (fun e -> events := e :: !events);
  let events = List.rev !events in
  (* MessageStart → ContentBlockStart → ContentBlockDelta → ContentBlockStop
     → MessageDelta → MessageStop = 6 events *)
  Alcotest.(check int) "6 events" 6 (List.length events);
  (match List.nth events 0 with
   | MessageStart { id; model; _ } ->
     Alcotest.(check string) "id" "msg_test" id;
     Alcotest.(check string) "model" "claude-sonnet-4" model
   | _ -> Alcotest.fail "expected MessageStart");
  (match List.nth events 1 with
   | ContentBlockStart { index; content_type; tool_id; tool_name } ->
     Alcotest.(check int) "index" 0 index;
     Alcotest.(check string) "type" "tool_use" content_type;
     Alcotest.(check bool) "tool_id" true (tool_id = Some "tu_1");
     Alcotest.(check bool) "tool_name" true (tool_name = Some "extract_person")
   | _ -> Alcotest.fail "expected ContentBlockStart");
  (match List.nth events 2 with
   | ContentBlockDelta { index; delta = InputJsonDelta _ } ->
     Alcotest.(check int) "delta index" 0 index
   | _ -> Alcotest.fail "expected InputJsonDelta");
  (match List.nth events 3 with
   | ContentBlockStop { index } -> Alcotest.(check int) "stop" 0 index
   | _ -> Alcotest.fail "expected ContentBlockStop");
  (match List.nth events 4 with
   | MessageDelta { stop_reason; _ } ->
     Alcotest.(check bool) "stop_reason" true (stop_reason = Some StopToolUse)
   | _ -> Alcotest.fail "expected MessageDelta");
  (match List.nth events 5 with
   | MessageStop -> ()
   | _ -> Alcotest.fail "expected MessageStop")

(* ── 2. on_event_callback_fires ──────────────────────────────────── *)

let test_on_event_callback_fires () =
  let input_json = `Assoc [("name", `String "Eve"); ("age", `Int 30)] in
  let response =
    { id = "msg_2"; model = "claude-sonnet-4";
      stop_reason = StopToolUse;
      content = [Text "thinking..."; ToolUse { id = "tu_2"; name = "extract_person"; input = input_json }];
      usage = Some { input_tokens = 100; output_tokens = 50;
                     cache_creation_input_tokens = 0; cache_read_input_tokens = 0 ; cost_usd = None } }
  in
  let event_types = ref [] in
  Streaming.emit_synthetic_events response (fun e ->
    let t = match e with
      | MessageStart _ -> "message_start"
      | ContentBlockStart _ -> "content_block_start"
      | ContentBlockDelta _ -> "content_block_delta"
      | ContentBlockStop _ -> "content_block_stop"
      | MessageDelta _ -> "message_delta"
      | MessageStop -> "message_stop"
      | Ping -> "ping"
      | SSEError _ -> "error"
    in
    event_types := t :: !event_types);
  let types = List.rev !event_types in
  (* 2 content blocks × (start+delta+stop) = 6, plus MessageStart+MessageDelta+MessageStop = 9 *)
  Alcotest.(check int) "9 events for 2 blocks" 9 (List.length types);
  Alcotest.(check string) "first" "message_start" (List.hd types);
  Alcotest.(check string) "last" "message_stop" (List.nth types 8)

(* ── 3. tool_use_json_parseable ──────────────────────────────────── *)

let test_tool_use_json_parseable () =
  let input_json = `Assoc [("name", `String "Alice"); ("age", `Int 30)] in
  let response = make_tool_response ~tool_id:"tu_3"
    ~tool_name:"extract_person" ~input_json in
  let json_parts = ref [] in
  Streaming.emit_synthetic_events response (fun e ->
    match e with
    | ContentBlockDelta { delta = InputJsonDelta s; _ } ->
      json_parts := s :: !json_parts
    | _ -> ());
  let combined = String.concat "" (List.rev !json_parts) in
  (try
     let parsed = Yojson.Safe.from_string combined in
     let open Yojson.Safe.Util in
     Alcotest.(check string) "name" "Alice" (parsed |> member "name" |> to_string);
     Alcotest.(check int) "age" 30 (parsed |> member "age" |> to_int)
   with Yojson.Json_error e ->
     Alcotest.fail ("Invalid JSON: " ^ e))

(* ── 4. multiple_schemas ─────────────────────────────────────────── *)

let test_multiple_schemas () =
  let person_json = Structured.schema_to_tool_json person_schema in
  let color_json = Structured.schema_to_tool_json color_schema in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "person" "extract_person"
    (person_json |> member "name" |> to_string);
  Alcotest.(check string) "color" "extract_color"
    (color_json |> member "name" |> to_string);
  (* extract_tool_input picks the matching schema *)
  let content = [
    ToolUse { id = "tu_p"; name = "extract_person"; input = `Assoc [("name", `String "X"); ("age", `Int 1)] };
    ToolUse { id = "tu_c"; name = "extract_color"; input = `Assoc [("color", `String "blue")] };
  ] in
  (match Structured.extract_tool_input ~schema:color_schema content with
   | Ok color -> Alcotest.(check string) "color matched" "blue" color
   | Error e -> Alcotest.fail ("color error: " ^ Error.to_string e));
  (match Structured.extract_tool_input ~schema:person_schema content with
   | Ok (name, _) -> Alcotest.(check string) "person matched" "X" name
   | Error e -> Alcotest.fail ("person error: " ^ Error.to_string e))

(* ── 5. accumulate_json_deltas ───────────────────────────────────── *)

let test_accumulate_json_deltas () =
  let parts = [
    {|{"name"|};
    {|: "Alice"|};
    {|, "age": 30}|};
  ] in
  let buf = Buffer.create 64 in
  List.iter (fun part ->
    let data = make_delta_data 0 part in
    match Streaming.parse_sse_event None data with
    | Some (ContentBlockDelta { delta = InputJsonDelta s; _ }) ->
      Buffer.add_string buf s
    | _ -> Alcotest.fail "expected InputJsonDelta"
  ) parts;
  let combined = Buffer.contents buf in
  let parsed = Yojson.Safe.from_string combined in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "name" "Alice" (parsed |> member "name" |> to_string);
  Alcotest.(check int) "age" 30 (parsed |> member "age" |> to_int)

(* ── 6. accumulate_empty_delta ───────────────────────────────────── *)

let test_accumulate_empty_delta () =
  let data = make_delta_data 0 "" in
  match Streaming.parse_sse_event None data with
  | Some (ContentBlockDelta { delta = InputJsonDelta s; _ }) ->
    Alcotest.(check string) "empty partial" "" s
  | _ -> Alcotest.fail "expected InputJsonDelta for empty string"

(* ── 7. accumulate_partial_then_complete ─────────────────────────── *)

let test_accumulate_partial_then_complete () =
  let parts = [
    {|{"col|};
    {|or": "|};
    {|red"}|};
  ] in
  let buf = Buffer.create 64 in
  List.iter (fun part ->
    let data = make_delta_data 0 part in
    match Streaming.parse_sse_event None data with
    | Some (ContentBlockDelta { delta = InputJsonDelta s; _ }) ->
      Buffer.add_string buf s
    | _ -> Alcotest.fail "expected InputJsonDelta"
  ) parts;
  let combined = Buffer.contents buf in
  let parsed = Yojson.Safe.from_string combined in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "color" "red" (parsed |> member "color" |> to_string)

(* ── 8. extract_after_accumulation ───────────────────────────────── *)

let test_extract_after_accumulation () =
  (* Full roundtrip: schema → synthetic events → accumulate → parse *)
  let input_json = `Assoc [("name", `String "Zoe"); ("age", `Int 28)] in
  let response = make_tool_response ~tool_id:"tu_rt"
    ~tool_name:"extract_person" ~input_json in
  (* Step 1: emit synthetic events and accumulate InputJsonDelta *)
  let blocks : (int, Buffer.t) Hashtbl.t = Hashtbl.create 4 in
  let block_types : (int, string) Hashtbl.t = Hashtbl.create 4 in
  let block_tool_ids : (int, string) Hashtbl.t = Hashtbl.create 4 in
  let block_tool_names : (int, string) Hashtbl.t = Hashtbl.create 4 in
  Streaming.emit_synthetic_events response (fun evt ->
    match evt with
    | ContentBlockStart { index; content_type; tool_id; tool_name } ->
      Hashtbl.replace block_types index content_type;
      Hashtbl.replace blocks index (Buffer.create 64);
      (match tool_id with Some id -> Hashtbl.replace block_tool_ids index id | None -> ());
      (match tool_name with Some n -> Hashtbl.replace block_tool_names index n | None -> ())
    | ContentBlockDelta { index; delta } ->
      let buf = match Hashtbl.find_opt blocks index with
        | Some b -> b
        | None -> let b = Buffer.create 64 in Hashtbl.replace blocks index b; b
      in
      (match delta with
       | InputJsonDelta s -> Buffer.add_string buf s
       | TextDelta s -> Buffer.add_string buf s
       | ThinkingDelta s -> Buffer.add_string buf s)
    | _ -> ());
  (* Step 2: reconstruct content blocks (same as streaming.ml) *)
  let content =
    Hashtbl.fold (fun index ctype acc ->
      let text = match Hashtbl.find_opt blocks index with
        | Some buf -> Buffer.contents buf
        | None -> ""
      in
      let block = match ctype with
        | "tool_use" ->
          let tid = match Hashtbl.find_opt block_tool_ids index with
            | Some id -> id | None -> "" in
          let tname = match Hashtbl.find_opt block_tool_names index with
            | Some n -> n | None -> "" in
          (try Some (ToolUse { id = tid; name = tname; input = Yojson.Safe.from_string text })
           with Yojson.Json_error _ -> None)
        | "text" -> Some (Text text)
        | _ -> None
      in
      match block with Some b -> (index, b) :: acc | None -> acc
    ) block_types []
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.map snd
  in
  (* Step 3: extract using schema *)
  match Structured.extract_tool_input ~schema:person_schema content with
  | Ok (name, age) ->
    Alcotest.(check string) "roundtrip name" "Zoe" name;
    Alcotest.(check int) "roundtrip age" 28 age
  | Error e -> Alcotest.fail ("roundtrip error: " ^ Error.to_string e)

(* ── Suite ────────────────────────────────────────────────────────── *)

let () =
  Alcotest.run "structured_stream" [
    "synthetic_events", [
      Alcotest.test_case "tool_use sequence" `Quick test_synthetic_events_for_tool_use;
      Alcotest.test_case "callback fires" `Quick test_on_event_callback_fires;
      Alcotest.test_case "json parseable" `Quick test_tool_use_json_parseable;
    ];
    "schema_selection", [
      Alcotest.test_case "multiple schemas" `Quick test_multiple_schemas;
    ];
    "delta_accumulation", [
      Alcotest.test_case "multi-fragment" `Quick test_accumulate_json_deltas;
      Alcotest.test_case "empty delta" `Quick test_accumulate_empty_delta;
      Alcotest.test_case "partial then complete" `Quick test_accumulate_partial_then_complete;
    ];
    "roundtrip", [
      Alcotest.test_case "extract after accumulation" `Quick test_extract_after_accumulation;
    ];
  ]
