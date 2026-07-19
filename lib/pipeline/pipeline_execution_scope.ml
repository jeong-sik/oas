open Result_syntax

type t =
  { turn : Execution_agent_scope.turn option
  ; mutable provider : Execution_agent_scope.provider_attempt option
  }

let sdk_error error = Error.Internal (Execution_agent_scope.error_to_string error)

(* A [Closed Succeeded] turn/provider found at a resume ordinal under a still-open
   run: an idempotent completed boundary. [Settled_provider_closed] carries the
   still-open turn (window: crash inside [close_success] after the provider close
   but before the turn close) so the caller can finish that interrupted close;
   [Settled_turn_closed] needs no further journal write (window: crash after the
   turn close but before the root finish). *)
type settled_boundary =
  | Settled_turn_closed
  | Settled_provider_closed of Execution_agent_scope.turn

type resumed =
  | Fresh
  | Active of t
  | Settled of settled_boundary

let open_turn scope ~ordinal =
  match scope with
  | None -> Ok { turn = None; provider = None }
  | Some scope ->
    (match Execution_agent_scope.resume_turn scope ~ordinal with
     | Error error -> Error (sdk_error error)
     | Ok (Resume_turn_open turn) -> Ok { turn = Some turn; provider = None }
     | Ok Resume_turn_absent ->
       Execution_agent_scope.open_turn scope ~ordinal
       |> Result.map (fun turn -> { turn = Some turn; provider = None })
       |> Result.map_error sdk_error
     | Ok Resume_turn_settled ->
       (* A fresh turn ordinal must not already be a settled boundary; a collision
          is inconsistent topology rather than reopening durably-settled work. *)
       Error
         (sdk_error
            (Execution_agent_scope.Resume_topology_mismatch
               "fresh turn ordinal is already settled")))
;;

let resume_current scope ~ordinal =
  match scope with
  | None -> Ok Fresh
  | Some scope ->
    (match Execution_agent_scope.resume_turn scope ~ordinal with
     | Error error -> Error (sdk_error error)
     | Ok Resume_turn_absent -> Ok Fresh
     | Ok Resume_turn_settled -> Ok (Settled Settled_turn_closed)
     | Ok (Resume_turn_open turn) ->
       (match Execution_agent_scope.resume_provider_attempt turn with
        | Error error -> Error (sdk_error error)
        | Ok Resume_provider_absent -> Ok Fresh
        | Ok Resume_provider_settled -> Ok (Settled (Settled_provider_closed turn))
        | Ok (Resume_provider_open provider) ->
          Ok (Active { turn = Some turn; provider = Some provider })))
;;

let finalize_settled = function
  | Settled_turn_closed -> Ok ()
  | Settled_provider_closed turn ->
    Execution_agent_scope.close_turn turn Execution_event.Succeeded
    |> Result.map_error sdk_error
;;

let before_provider_attempt t binding =
  match t.turn with
  | None -> Ok ()
  | Some turn ->
    Execution_agent_scope.open_provider_attempt turn ~ordinal:0 binding
    |> Result.map (fun provider -> t.provider <- Some provider)
    |> Result.map_error sdk_error
;;

let provider t = t.provider

let invocations_settled t =
  match t.provider with
  | None -> Ok false
  | Some provider ->
    Execution_agent_scope.provider_invocations_settled provider
    |> Result.map_error sdk_error
;;

let close_success t =
  match t.provider, t.turn with
  | None, None -> Ok ()
  | Some provider, Some turn ->
    let* () =
      Execution_agent_scope.close_provider_attempt provider Execution_event.Succeeded
      |> Result.map_error sdk_error
    in
    Execution_agent_scope.close_turn turn Execution_event.Succeeded
    |> Result.map_error sdk_error
  | None, Some _ | Some _, None ->
    Error (sdk_error Execution_agent_scope.Invocation_locator_mismatch)
;;
