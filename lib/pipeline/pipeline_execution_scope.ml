open Result_syntax

type t =
  { turn : Execution_agent_scope.turn option
  ; mutable provider : Execution_agent_scope.provider_attempt option
  }

let sdk_error error = Error.Internal (Execution_agent_scope.error_to_string error)

let open_turn scope ~ordinal =
  match scope with
  | None -> Ok { turn = None; provider = None }
  | Some scope ->
    Execution_agent_scope.open_turn scope ~ordinal
    |> Result.map (fun turn -> { turn = Some turn; provider = None })
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
