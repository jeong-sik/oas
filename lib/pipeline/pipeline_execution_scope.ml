open Result_syntax

type turn_authority =
  | Transient of int
  | Durable of Execution_agent_scope.turn

type t =
  { turn : turn_authority
  ; mutable provider : Execution_agent_scope.provider_attempt option
  }

let sdk_error error = Error.Internal (Execution_agent_scope.error_to_string error)

let open_turn scope ~ordinal =
  match scope with
  | None -> Ok { turn = Transient ordinal; provider = None }
  | Some scope ->
    (match Execution_agent_scope.resume_turn scope ~ordinal with
     | Error error -> Error (sdk_error error)
     | Ok (Some turn) -> Ok { turn = Durable turn; provider = None }
     | Ok None ->
       Execution_agent_scope.open_turn scope ~ordinal
       |> Result.map (fun turn -> { turn = Durable turn; provider = None })
       |> Result.map_error sdk_error)
;;

let resume_current scope =
  match scope with
  | None -> Ok None
  | Some scope ->
    (match Execution_agent_scope.resume_open_turn scope with
     | Error error -> Error (sdk_error error)
     | Ok None -> Ok None
     | Ok (Some turn) ->
       Execution_agent_scope.resume_provider_attempt turn
       |> Result.map (function
         | None -> None
         | Some provider -> Some { turn = Durable turn; provider = Some provider })
       |> Result.map_error sdk_error)
;;

let turn_ordinal t =
  match t.turn with
  | Transient ordinal -> ordinal
  | Durable turn -> Execution_agent_scope.turn_ordinal turn
;;

let before_provider_attempt t binding =
  match t.turn with
  | Transient _ -> Ok ()
  | Durable turn ->
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
  | None, Transient _ -> Ok ()
  | Some provider, Durable turn ->
    let* () =
      Execution_agent_scope.close_provider_attempt provider Execution_event.Succeeded
      |> Result.map_error sdk_error
    in
    Execution_agent_scope.close_turn turn Execution_event.Succeeded
    |> Result.map_error sdk_error
  | None, Durable _ | Some _, Transient _ ->
    Error (sdk_error Execution_agent_scope.Invocation_locator_mismatch)
;;
