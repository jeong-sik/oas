(** Test helper factories for swarm testing — zero-LLM mock patterns.

    @since 0.43.0 *)

open Agent_sdk

(** Create a mock api_response with the given text. *)
let text_response text : (Types.api_response, Error.sdk_error) result =
  Ok {
    Types.id = "mock-id";
    model = "mock-model";
    stop_reason = EndTurn;
    content = [Text text];
    usage = None;
  }

(** Create a mock api_response that returns an error. *)
let error_response msg : (Types.api_response, Error.sdk_error) result =
  Error (Error.Agent (MaxTurnsExceeded { turns = 1; limit = 1 }))
  |> (fun _ -> Error (Error.Internal msg))

(** Create a mock agent_entry that always returns the given text. *)
let mock_entry ~name ?(role = Swarm_types.Execute) text : Swarm_types.agent_entry =
  { name;
    run = (fun ~sw:_ _prompt -> text_response text);
    role;
  }

(** Create a mock agent_entry that fails with the given message. *)
let failing_entry ~name ?(role = Swarm_types.Execute) msg : Swarm_types.agent_entry =
  { name;
    run = (fun ~sw:_ _prompt -> Error (Error.Internal msg));
    role;
  }

(** Create a mock agent_entry with a counter (tracks call count). *)
let counting_entry ~name ?(role = Swarm_types.Execute) text
    : Swarm_types.agent_entry * (unit -> int) =
  let count = ref 0 in
  let entry : Swarm_types.agent_entry = {
    name;
    run = (fun ~sw:_ _prompt -> incr count; text_response text);
    role;
  } in
  (entry, fun () -> !count)

(** Create a basic swarm_config for testing with given entries. *)
let basic_config ~prompt entries : Swarm_types.swarm_config =
  { entries;
    mode = Decentralized;
    convergence = None;
    max_parallel = List.length entries;
    prompt;
    timeout_sec = None;
    budget = Swarm_types.no_budget;
  }
