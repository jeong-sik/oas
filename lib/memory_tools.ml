type backend =
  | Plain of Memory.t
  | Acl of { acl: Memory_access.t; agent_name: string }

let json_string value = Yojson.Safe.to_string value

let ok_json value = Ok { Types.content = json_string value }

let tool_error message =
  Error { Types.message = message; recoverable = true }

let bind result f =
  match result with
  | Ok value -> f value
  | Error _ as err -> err

let tier_to_string = function
  | Memory.Scratchpad -> "scratchpad"
  | Memory.Working -> "working"
  | Memory.Episodic -> "episodic"
  | Memory.Procedural -> "procedural"
  | Memory.Long_term -> "long_term"

let parse_string_field json name =
  match Yojson.Safe.Util.member name json with
  | `String value -> Ok value
  | `Null -> tool_error (Printf.sprintf "missing '%s' parameter" name)
  | _ -> tool_error (Printf.sprintf "expected '%s' to be a string" name)

let parse_optional_string_field json name =
  match Yojson.Safe.Util.member name json with
  | `String value -> Ok (Some value)
  | `Null -> Ok None
  | _ -> tool_error (Printf.sprintf "expected '%s' to be a string" name)

let parse_bool_field json name ~default =
  match Yojson.Safe.Util.member name json with
  | `Bool value -> Ok value
  | `Null -> Ok default
  | _ -> tool_error (Printf.sprintf "expected '%s' to be a boolean" name)

let parse_float_field json name ~default =
  match Yojson.Safe.Util.member name json with
  | `Float value -> Ok value
  | `Int value -> Ok (float_of_int value)
  | `Null -> Ok default
  | _ -> tool_error (Printf.sprintf "expected '%s' to be a number" name)

let parse_generic_tier json ~default =
  let parse value =
    match String.lowercase_ascii value with
    | "scratchpad" -> Ok Memory.Scratchpad
    | "working" -> Ok Memory.Working
    | "long_term" | "long-term" -> Ok Memory.Long_term
    | "episodic" | "procedural" ->
      tool_error
        "generic memory tools only support scratchpad, working, and long_term"
    | _ ->
      tool_error
        "invalid 'tier'; expected one of scratchpad, working, or long_term"
  in
  match Yojson.Safe.Util.member "tier" json with
  | `String value -> parse value
  | `Null -> Ok default
  | _ -> tool_error "expected 'tier' to be a string"

let parse_string_list_field json name =
  match Yojson.Safe.Util.member name json with
  | `Null -> Ok []
  | `List values ->
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | `String value :: rest -> loop (value :: acc) rest
      | _ -> tool_error (Printf.sprintf "expected '%s' to be an array of strings" name)
    in
    loop [] values
  | _ -> tool_error (Printf.sprintf "expected '%s' to be an array of strings" name)

let parse_metadata_field json =
  match Yojson.Safe.Util.member "metadata" json with
  | `Null -> Ok []
  | `Assoc pairs -> Ok pairs
  | _ -> tool_error "expected 'metadata' to be an object"

let parse_value_json json =
  bind (parse_string_field json "value_json") (fun raw ->
      try Ok (Yojson.Safe.from_string raw)
      with Yojson.Json_error _ -> Ok (`String raw))

let parse_outcome json =
  let detail_result = parse_optional_string_field json "detail" in
  bind detail_result (fun detail ->
      match Yojson.Safe.Util.member "outcome" json with
      | `Null -> Ok Memory.Neutral
      | `String "success" ->
        Ok (Memory.Success (Option.value detail ~default:""))
      | `String "failure" ->
        Ok (Memory.Failure (Option.value detail ~default:""))
      | `String "neutral" -> Ok Memory.Neutral
      | `String _ ->
        tool_error "expected 'outcome' to be success, failure, or neutral"
      | _ ->
        tool_error "expected 'outcome' to be a string")

let procedure_to_json (proc : Memory.procedure) =
  `Assoc
    [
      ("id", `String proc.id);
      ("pattern", `String proc.pattern);
      ("action", `String proc.action);
      ("success_count", `Int proc.success_count);
      ("failure_count", `Int proc.failure_count);
      ("confidence", `Float proc.confidence);
      ("last_used", `Float proc.last_used);
      ("metadata", `Assoc proc.metadata);
    ]

let generated_episode_id () =
  let millis = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
  Printf.sprintf "ep_%Ld" millis

let store_value backend ~tier key value =
  match backend with
  | Plain mem ->
    (match Memory.store mem ~tier key value with
     | Ok () -> Ok ()
     | Error reason -> tool_error reason)
  | Acl { acl; agent_name } ->
    (match Memory_access.store acl ~agent:agent_name ~tier key value with
    | Ok () -> Ok ()
    | Error err -> tool_error (Memory_access.access_error_to_string err))

let recall_value backend ~tier key ~exact =
  match backend with
  | Plain mem ->
    Ok
      (if exact then Memory.recall_exact mem ~tier key
       else Memory.recall mem ~tier key)
  | Acl { acl; agent_name } ->
    let result =
      if exact then Memory_access.recall_exact acl ~agent:agent_name ~tier key
      else Memory_access.recall acl ~agent:agent_name ~tier key
    in
    (match result with
    | Ok value -> Ok value
    | Error err -> tool_error (Memory_access.access_error_to_string err))

let store_episode backend episode =
  match backend with
  | Plain mem ->
    Memory.store_episode mem episode;
    Ok ()
  | Acl { acl; agent_name } ->
    (match Memory_access.store_episode acl ~agent:agent_name episode with
    | Ok () -> Ok ()
    | Error err -> tool_error (Memory_access.access_error_to_string err))

let find_procedure_backend backend ~pattern ~min_confidence ~touch =
  match backend with
  | Plain mem -> Ok (Memory.find_procedure mem ~pattern ~min_confidence ~touch ())
  | Acl { acl; agent_name } ->
    (match
       Memory_access.find_procedure acl ~agent:agent_name ~pattern
         ~min_confidence ~touch ()
     with
    | Ok value -> Ok value
    | Error err -> tool_error (Memory_access.access_error_to_string err))

let remember_tool backend =
  Tool.create ~name:"memory_remember"
    ~description:
      "Store a JSON value in scratchpad, working, or long_term memory."
    ~parameters:
      [
        {
          Types.name = "tier";
          description = "Optional tier: scratchpad, working, or long_term";
          param_type = Types.String;
          required = false;
        };
        {
          Types.name = "key";
          description = "Memory key";
          param_type = Types.String;
          required = true;
        };
        {
          Types.name = "value_json";
          description = "JSON string to store; invalid JSON is stored as a raw string";
          param_type = Types.String;
          required = true;
        };
      ]
    (fun input ->
      bind (parse_generic_tier input ~default:Memory.Working) (fun tier ->
          bind (parse_string_field input "key") (fun key ->
              bind (parse_value_json input) (fun value ->
                  bind (store_value backend ~tier key value) (fun () ->
                      ok_json
                        (`Assoc
                          [
                            ("ok", `Bool true);
                            ("tier", `String (tier_to_string tier));
                            ("key", `String key);
                          ]))))))

let recall_tool backend =
  Tool.create ~name:"memory_recall"
    ~description:
      "Recall a memory value from scratchpad, working, or long_term memory."
    ~parameters:
      [
        {
          Types.name = "tier";
          description = "Optional tier: scratchpad, working, or long_term";
          param_type = Types.String;
          required = false;
        };
        {
          Types.name = "key";
          description = "Memory key";
          param_type = Types.String;
          required = true;
        };
        {
          Types.name = "exact";
          description = "Use exact tier lookup without fallback";
          param_type = Types.Boolean;
          required = false;
        };
      ]
    (fun input ->
      bind (parse_generic_tier input ~default:Memory.Working) (fun tier ->
          bind (parse_string_field input "key") (fun key ->
              bind (parse_bool_field input "exact" ~default:false) (fun exact ->
                  bind (recall_value backend ~tier key ~exact) (fun value ->
                      ok_json
                        (`Assoc
                          [
                            ("found", `Bool (Option.is_some value));
                            ("tier", `String (tier_to_string tier));
                            ("key", `String key);
                            ("exact", `Bool exact);
                            ( "value",
                              match value with Some value -> value | None -> `Null );
                          ]))))))

let remember_episode_tool backend =
  Tool.create ~name:"memory_remember_episode"
    ~description:"Store a structured episodic memory record."
    ~parameters:
      [
        {
          Types.name = "id";
          description = "Optional episode id; autogenerated when omitted";
          param_type = Types.String;
          required = false;
        };
        {
          Types.name = "action";
          description = "What happened";
          param_type = Types.String;
          required = true;
        };
        {
          Types.name = "participants";
          description = "Optional participant names";
          param_type = Types.Array;
          required = false;
        };
        {
          Types.name = "outcome";
          description = "Optional outcome: success, failure, or neutral";
          param_type = Types.String;
          required = false;
        };
        {
          Types.name = "detail";
          description = "Optional detail for success or failure outcomes";
          param_type = Types.String;
          required = false;
        };
        {
          Types.name = "salience";
          description = "Optional salience score";
          param_type = Types.Number;
          required = false;
        };
        {
          Types.name = "metadata";
          description = "Optional metadata object";
          param_type = Types.Object;
          required = false;
        };
      ]
    (fun input ->
      bind (parse_optional_string_field input "id") (fun id ->
          bind (parse_string_field input "action") (fun action ->
              bind (parse_string_list_field input "participants") (fun participants ->
                  bind (parse_outcome input) (fun outcome ->
                      bind (parse_float_field input "salience" ~default:0.7) (fun salience ->
                          bind (parse_metadata_field input) (fun metadata ->
                              let episode : Memory.episode =
                                {
                                  id = Option.value id ~default:(generated_episode_id ());
                                  timestamp = Unix.gettimeofday ();
                                  participants;
                                  action;
                                  outcome;
                                  salience;
                                  metadata;
                                }
                              in
                              bind (store_episode backend episode) (fun () ->
                                  ok_json
                                    (`Assoc
                                      [
                                        ("ok", `Bool true);
                                        ("tier", `String "episodic");
                                        ("id", `String episode.id);
                                      ])))))))))

let find_procedure_tool backend =
  Tool.create ~name:"memory_find_procedure"
    ~description:"Find the best procedural memory matching a pattern."
    ~parameters:
      [
        {
          Types.name = "pattern";
          description = "Pattern substring to search";
          param_type = Types.String;
          required = true;
        };
        {
          Types.name = "min_confidence";
          description = "Optional minimum confidence threshold";
          param_type = Types.Number;
          required = false;
        };
        {
          Types.name = "touch";
          description = "Update last_used on the selected procedure";
          param_type = Types.Boolean;
          required = false;
        };
      ]
    (fun input ->
      bind (parse_string_field input "pattern") (fun pattern ->
          bind (parse_float_field input "min_confidence" ~default:0.0) (fun min_confidence ->
              bind (parse_bool_field input "touch" ~default:false) (fun touch ->
                  bind
                    (find_procedure_backend backend ~pattern ~min_confidence ~touch)
                    (fun procedure ->
                      ok_json
                        (`Assoc
                          [
                            ("found", `Bool (Option.is_some procedure));
                            ("pattern", `String pattern);
                            ( "procedure",
                              match procedure with
                              | Some procedure -> procedure_to_json procedure
                              | None -> `Null );
                          ]))))))

let remember mem = remember_tool (Plain mem)
let recall mem = recall_tool (Plain mem)
let remember_episode mem = remember_episode_tool (Plain mem)
let find_procedure mem = find_procedure_tool (Plain mem)
let all mem = [ remember mem; recall mem; remember_episode mem; find_procedure mem ]

let remember_acl acl ~agent_name = remember_tool (Acl { acl; agent_name })
let recall_acl acl ~agent_name = recall_tool (Acl { acl; agent_name })
let remember_episode_acl acl ~agent_name =
  remember_episode_tool (Acl { acl; agent_name })
let find_procedure_acl acl ~agent_name =
  find_procedure_tool (Acl { acl; agent_name })

let all_acl acl ~agent_name =
  [
    remember_acl acl ~agent_name;
    recall_acl acl ~agent_name;
    remember_episode_acl acl ~agent_name;
    find_procedure_acl acl ~agent_name;
  ]

[@@@coverage off]
(* === Inline tests === *)

(* --- json_string / ok_json / tool_error --- *)

let%test "json_string encodes value" =
  json_string (`String "hello") = "\"hello\""

let%test "json_string encodes int" =
  json_string (`Int 42) = "42"

let%test "ok_json wraps as tool result" =
  match ok_json (`Bool true) with
  | Ok { Types.content } -> content = "true"
  | Error _ -> false

let%test "tool_error returns recoverable error" =
  match tool_error "bad input" with
  | Error { Types.message = "bad input"; recoverable = true } -> true
  | _ -> false

(* --- bind --- *)

let%test "bind ok" =
  bind (Ok 1) (fun x -> Ok (x + 1)) = Ok 2

let%test "bind error propagates" =
  let err = Error { Types.message = "fail"; recoverable = true } in
  match bind err (fun _ -> Ok 0) with
  | Error { message = "fail"; _ } -> true
  | _ -> false

(* --- tier_to_string --- *)

let%test "tier_to_string scratchpad" =
  tier_to_string Memory.Scratchpad = "scratchpad"

let%test "tier_to_string working" =
  tier_to_string Memory.Working = "working"

let%test "tier_to_string episodic" =
  tier_to_string Memory.Episodic = "episodic"

let%test "tier_to_string procedural" =
  tier_to_string Memory.Procedural = "procedural"

let%test "tier_to_string long_term" =
  tier_to_string Memory.Long_term = "long_term"

(* --- parse_string_field --- *)

let%test "parse_string_field present" =
  let json = `Assoc [("key", `String "val")] in
  parse_string_field json "key" = Ok "val"

let%test "parse_string_field missing" =
  let json = `Assoc [] in
  match parse_string_field json "key" with
  | Error _ -> true
  | Ok _ -> false

let%test "parse_string_field null" =
  let json = `Assoc [("key", `Null)] in
  match parse_string_field json "key" with
  | Error _ -> true
  | Ok _ -> false

let%test "parse_string_field wrong type" =
  let json = `Assoc [("key", `Int 42)] in
  match parse_string_field json "key" with
  | Error _ -> true
  | Ok _ -> false

(* --- parse_optional_string_field --- *)

let%test "parse_optional_string_field present" =
  let json = `Assoc [("key", `String "val")] in
  parse_optional_string_field json "key" = Ok (Some "val")

let%test "parse_optional_string_field null" =
  let json = `Assoc [("key", `Null)] in
  parse_optional_string_field json "key" = Ok None

let%test "parse_optional_string_field missing" =
  let json = `Assoc [] in
  parse_optional_string_field json "key" = Ok None

let%test "parse_optional_string_field wrong type" =
  let json = `Assoc [("key", `Int 42)] in
  match parse_optional_string_field json "key" with
  | Error _ -> true
  | Ok _ -> false

(* --- parse_bool_field --- *)

let%test "parse_bool_field present true" =
  let json = `Assoc [("flag", `Bool true)] in
  parse_bool_field json "flag" ~default:false = Ok true

let%test "parse_bool_field present false" =
  let json = `Assoc [("flag", `Bool false)] in
  parse_bool_field json "flag" ~default:true = Ok false

let%test "parse_bool_field null uses default" =
  let json = `Assoc [("flag", `Null)] in
  parse_bool_field json "flag" ~default:true = Ok true

let%test "parse_bool_field missing uses default" =
  let json = `Assoc [] in
  parse_bool_field json "flag" ~default:false = Ok false

let%test "parse_bool_field wrong type" =
  let json = `Assoc [("flag", `String "yes")] in
  match parse_bool_field json "flag" ~default:false with
  | Error _ -> true
  | Ok _ -> false

(* --- parse_float_field --- *)

let%test "parse_float_field float" =
  let json = `Assoc [("val", `Float 3.14)] in
  parse_float_field json "val" ~default:0.0 = Ok 3.14

let%test "parse_float_field int coerced to float" =
  let json = `Assoc [("val", `Int 42)] in
  parse_float_field json "val" ~default:0.0 = Ok 42.0

let%test "parse_float_field null uses default" =
  let json = `Assoc [] in
  parse_float_field json "val" ~default:1.5 = Ok 1.5

let%test "parse_float_field wrong type" =
  let json = `Assoc [("val", `String "nope")] in
  match parse_float_field json "val" ~default:0.0 with
  | Error _ -> true
  | Ok _ -> false

(* --- parse_generic_tier --- *)

let%test "parse_generic_tier scratchpad" =
  let json = `Assoc [("tier", `String "scratchpad")] in
  parse_generic_tier json ~default:Memory.Working = Ok Memory.Scratchpad

let%test "parse_generic_tier working" =
  let json = `Assoc [("tier", `String "working")] in
  parse_generic_tier json ~default:Memory.Scratchpad = Ok Memory.Working

let%test "parse_generic_tier long_term" =
  let json = `Assoc [("tier", `String "long_term")] in
  parse_generic_tier json ~default:Memory.Working = Ok Memory.Long_term

let%test "parse_generic_tier long-term hyphen" =
  let json = `Assoc [("tier", `String "long-term")] in
  parse_generic_tier json ~default:Memory.Working = Ok Memory.Long_term

let%test "parse_generic_tier episodic rejected" =
  let json = `Assoc [("tier", `String "episodic")] in
  match parse_generic_tier json ~default:Memory.Working with
  | Error _ -> true
  | Ok _ -> false

let%test "parse_generic_tier procedural rejected" =
  let json = `Assoc [("tier", `String "procedural")] in
  match parse_generic_tier json ~default:Memory.Working with
  | Error _ -> true
  | Ok _ -> false

let%test "parse_generic_tier invalid string" =
  let json = `Assoc [("tier", `String "garbage")] in
  match parse_generic_tier json ~default:Memory.Working with
  | Error _ -> true
  | Ok _ -> false

let%test "parse_generic_tier null uses default" =
  let json = `Assoc [] in
  parse_generic_tier json ~default:Memory.Long_term = Ok Memory.Long_term

let%test "parse_generic_tier wrong type" =
  let json = `Assoc [("tier", `Int 1)] in
  match parse_generic_tier json ~default:Memory.Working with
  | Error _ -> true
  | Ok _ -> false

let%test "parse_generic_tier uppercase scratchpad" =
  let json = `Assoc [("tier", `String "Scratchpad")] in
  parse_generic_tier json ~default:Memory.Working = Ok Memory.Scratchpad

(* --- parse_string_list_field --- *)

let%test "parse_string_list_field empty list" =
  let json = `Assoc [("tags", `List [])] in
  parse_string_list_field json "tags" = Ok []

let%test "parse_string_list_field with strings" =
  let json = `Assoc [("tags", `List [`String "a"; `String "b"])] in
  parse_string_list_field json "tags" = Ok ["a"; "b"]

let%test "parse_string_list_field null returns empty" =
  let json = `Assoc [] in
  parse_string_list_field json "tags" = Ok []

let%test "parse_string_list_field non-string element" =
  let json = `Assoc [("tags", `List [`Int 1])] in
  match parse_string_list_field json "tags" with
  | Error _ -> true
  | Ok _ -> false

let%test "parse_string_list_field wrong type" =
  let json = `Assoc [("tags", `String "not a list")] in
  match parse_string_list_field json "tags" with
  | Error _ -> true
  | Ok _ -> false

(* --- parse_metadata_field --- *)

let%test "parse_metadata_field object" =
  let json = `Assoc [("metadata", `Assoc [("k", `String "v")])] in
  parse_metadata_field json = Ok [("k", `String "v")]

let%test "parse_metadata_field null" =
  let json = `Assoc [] in
  parse_metadata_field json = Ok []

let%test "parse_metadata_field wrong type" =
  let json = `Assoc [("metadata", `List [])] in
  match parse_metadata_field json with
  | Error _ -> true
  | Ok _ -> false

(* --- parse_value_json --- *)

let%test "parse_value_json valid json" =
  let json = `Assoc [("value_json", `String "{\"a\":1}")] in
  match parse_value_json json with
  | Ok (`Assoc [("a", `Int 1)]) -> true
  | _ -> false

let%test "parse_value_json invalid json stored as raw string" =
  let json = `Assoc [("value_json", `String "not json")] in
  match parse_value_json json with
  | Ok (`String "not json") -> true
  | _ -> false

let%test "parse_value_json missing field" =
  let json = `Assoc [] in
  match parse_value_json json with
  | Error _ -> true
  | Ok _ -> false

(* --- parse_outcome --- *)

let%test "parse_outcome success" =
  let json = `Assoc [("outcome", `String "success"); ("detail", `String "done")] in
  parse_outcome json = Ok (Memory.Success "done")

let%test "parse_outcome failure" =
  let json = `Assoc [("outcome", `String "failure"); ("detail", `String "bad")] in
  parse_outcome json = Ok (Memory.Failure "bad")

let%test "parse_outcome neutral" =
  let json = `Assoc [("outcome", `String "neutral")] in
  parse_outcome json = Ok Memory.Neutral

let%test "parse_outcome null" =
  let json = `Assoc [] in
  parse_outcome json = Ok Memory.Neutral

let%test "parse_outcome invalid string" =
  let json = `Assoc [("outcome", `String "unknown")] in
  match parse_outcome json with
  | Error _ -> true
  | Ok _ -> false

let%test "parse_outcome wrong type" =
  let json = `Assoc [("outcome", `Int 1)] in
  match parse_outcome json with
  | Error _ -> true
  | Ok _ -> false

let%test "parse_outcome success no detail" =
  let json = `Assoc [("outcome", `String "success")] in
  parse_outcome json = Ok (Memory.Success "")

(* --- procedure_to_json --- *)

let%test "procedure_to_json roundtrip fields" =
  let proc : Memory.procedure = {
    id = "p1"; pattern = "build"; action = "run make";
    success_count = 5; failure_count = 1;
    confidence = 0.83; last_used = 1000.0;
    metadata = [("env", `String "prod")];
  } in
  let json = procedure_to_json proc in
  let open Yojson.Safe.Util in
  json |> member "id" |> to_string = "p1"
  && json |> member "pattern" |> to_string = "build"
  && json |> member "success_count" |> to_int = 5

(* --- generated_episode_id --- *)

let%test "generated_episode_id starts with ep_" =
  let id = generated_episode_id () in
  String.length id > 3 && String.sub id 0 3 = "ep_"

(* --- remember_tool / recall_tool / all tools --- *)

let%test "all returns 4 tools" =
  let mem = Memory.create () in
  List.length (all mem) = 4

let%test "all_acl returns 4 tools" =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  List.length (all_acl acl ~agent_name:"test") = 4

let%test "remember_tool stores and recall_tool retrieves" =
  let mem = Memory.create () in
  let rem = remember mem in
  let rec_ = recall mem in
  let store_input = `Assoc [
    ("key", `String "test_key");
    ("value_json", `String "{\"v\":1}");
  ] in
  (match Tool.execute rem store_input with
   | Ok _ ->
     let recall_input = `Assoc [
       ("key", `String "test_key");
     ] in
     (match Tool.execute rec_ recall_input with
      | Ok { content } ->
        let json = Yojson.Safe.from_string content in
        let open Yojson.Safe.Util in
        json |> member "found" |> to_bool = true
      | Error _ -> false)
   | Error _ -> false)

let%test "recall_tool missing key" =
  let mem = Memory.create () in
  let rec_ = recall mem in
  let input = `Assoc [("key", `String "nonexistent")] in
  match Tool.execute rec_ input with
  | Ok { content } ->
    let json = Yojson.Safe.from_string content in
    let open Yojson.Safe.Util in
    json |> member "found" |> to_bool = false
  | Error _ -> false

let%test "remember_tool with explicit tier" =
  let mem = Memory.create () in
  let rem = remember mem in
  let input = `Assoc [
    ("tier", `String "scratchpad");
    ("key", `String "sp_key");
    ("value_json", `String "\"hello\"");
  ] in
  match Tool.execute rem input with
  | Ok { content } ->
    let json = Yojson.Safe.from_string content in
    let open Yojson.Safe.Util in
    json |> member "ok" |> to_bool = true
    && json |> member "tier" |> to_string = "scratchpad"
  | Error _ -> false

let%test "recall_tool with exact=true" =
  let mem = Memory.create () in
  let rem = remember mem in
  let rec_ = recall mem in
  ignore (Tool.execute rem (`Assoc [
    ("tier", `String "working");
    ("key", `String "exact_key");
    ("value_json", `String "\"data\"");
  ]));
  let input = `Assoc [
    ("tier", `String "working");
    ("key", `String "exact_key");
    ("exact", `Bool true);
  ] in
  match Tool.execute rec_ input with
  | Ok { content } ->
    let json = Yojson.Safe.from_string content in
    let open Yojson.Safe.Util in
    json |> member "found" |> to_bool = true
    && json |> member "exact" |> to_bool = true
  | Error _ -> false
