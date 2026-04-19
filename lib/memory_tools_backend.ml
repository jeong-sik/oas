(** Backend operations shared by the public memory tool factories. *)

type t =
  | Plain of Memory.t
  | Acl of { acl: Memory_access.t; agent_name: string }

let tool_error = Memory_tools_parse.tool_error

let json_string value = Yojson.Safe.to_string value

let ok_json value = Ok { Types.content = json_string value }

let tier_to_string = function
  | Memory.Scratchpad -> "scratchpad"
  | Memory.Working -> "working"
  | Memory.Episodic -> "episodic"
  | Memory.Procedural -> "procedural"
  | Memory.Long_term -> "long_term"

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
  | Plain mem -> (
      match Memory.store mem ~tier key value with
      | Ok () -> Ok ()
      | Error reason -> tool_error reason)
  | Acl { acl; agent_name } -> (
      match Memory_access.store acl ~agent:agent_name ~tier key value with
      | Ok () -> Ok ()
      | Error err -> tool_error (Memory_access.access_error_to_string err))

let recall_value backend ~tier key ~exact =
  match backend with
  | Plain mem ->
      Ok
        (if exact then Memory.recall_exact mem ~tier key
         else Memory.recall mem ~tier key)
  | Acl { acl; agent_name } -> (
      let result =
        if exact then Memory_access.recall_exact acl ~agent:agent_name ~tier key
        else Memory_access.recall acl ~agent:agent_name ~tier key
      in
      match result with
      | Ok value -> Ok value
      | Error err -> tool_error (Memory_access.access_error_to_string err))

let store_episode backend episode =
  match backend with
  | Plain mem ->
      Memory.store_episode mem episode;
      Ok ()
  | Acl { acl; agent_name } -> (
      match Memory_access.store_episode acl ~agent:agent_name episode with
      | Ok () -> Ok ()
      | Error err -> tool_error (Memory_access.access_error_to_string err))

let find_procedure backend ~pattern ~min_confidence ~touch =
  match backend with
  | Plain mem -> Ok (Memory.find_procedure mem ~pattern ~min_confidence ~touch ())
  | Acl { acl; agent_name } -> (
      match
        Memory_access.find_procedure acl ~agent:agent_name ~pattern
          ~min_confidence ~touch ()
      with
      | Ok value -> Ok value
      | Error err -> tool_error (Memory_access.access_error_to_string err))
