(** Async guardrails: parallel input/output validation via Eio fibers.

    Extends {!Guardrails} (tool filtering) with content validation:
    - {b Input validators} run before the LLM call (gate).
    - {b Output validators} run after the LLM call (parallel).

    All validators within a group run concurrently via [Eio.Fiber.all]
    inside a dedicated [Eio.Switch]. Cancellation propagates correctly.

    @since 0.67.0 *)

open Types

(** Input validator: checks messages before sending to the LLM. *)
type input_validator = {
  name: string;
  validate: message list -> (unit, string) result;
}

(** Output validator: checks the LLM response. *)
type output_validator = {
  name: string;
  validate: api_response -> (unit, string) result;
}

(** Result of a validation run. *)
type validation_result =
  | Pass
  | Fail of { validator_name: string; reason: string }

(** Configuration for async guardrails. *)
type t = {
  input_validators: input_validator list;
  output_validators: output_validator list;
}

let empty = {
  input_validators = [];
  output_validators = [];
}

(** Run all input validators concurrently.

    Creates a dedicated [Eio.Switch] for parallel execution.
    Returns the first failure found, or [Pass] if all succeed.
    Cancellation exceptions propagate correctly. *)
let run_input (validators : input_validator list) (messages : message list)
    : validation_result =
  if validators = [] then Pass
  else begin
    let results = Array.make (List.length validators) Pass in
    let fns = List.mapi (fun i (v : input_validator) ->
      fun () ->
        match v.validate messages with
        | Ok () -> ()
        | Error reason ->
          results.(i) <- Fail { validator_name = v.name; reason }
    ) validators in
    Eio.Switch.run ~name:"input_validators" (fun _sw ->
      Eio.Fiber.all fns);
    Array.to_list results
    |> List.find_opt (function Fail _ -> true | Pass -> false)
    |> Option.value ~default:Pass
  end

(** Run all output validators concurrently.

    Same parallel execution pattern as {!run_input}. *)
let run_output (validators : output_validator list) (response : api_response)
    : validation_result =
  if validators = [] then Pass
  else begin
    let results = Array.make (List.length validators) Pass in
    let fns = List.mapi (fun i (v : output_validator) ->
      fun () ->
        match v.validate response with
        | Ok () -> ()
        | Error reason ->
          results.(i) <- Fail { validator_name = v.name; reason }
    ) validators in
    Eio.Switch.run ~name:"output_validators" (fun _sw ->
      Eio.Fiber.all fns);
    Array.to_list results
    |> List.find_opt (function Fail _ -> true | Pass -> false)
    |> Option.value ~default:Pass
  end

(** Convenience: run input validation, then an action, then output validation.

    If input validation fails, the action is not executed.
    Returns [Ok response] on success, [Error validation_result] on failure. *)
let guarded ~(config : t) ~(messages : message list)
    ~(action : unit -> (api_response, 'e) result)
    : (api_response, [`Validation of validation_result | `Action of 'e]) result =
  match run_input config.input_validators messages with
  | Fail _ as f -> Error (`Validation f)
  | Pass ->
    match action () with
    | Error e -> Error (`Action e)
    | Ok response ->
      match run_output config.output_validators response with
      | Fail _ as f -> Error (`Validation f)
      | Pass -> Ok response

(** Format a validation result for logging. *)
let result_to_string = function
  | Pass -> "pass"
  | Fail { validator_name; reason } ->
    Printf.sprintf "FAIL [%s]: %s" validator_name reason
