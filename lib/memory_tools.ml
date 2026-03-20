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
    Memory.store mem ~tier key value;
    Ok ()
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
