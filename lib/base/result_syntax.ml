(** Result monadic binding operators.

    Centralized definitions for {!Result} binding operators used across
    the agent_sdk codebase. Open this module in files that use [let*]
    and [let+] for Result-based computation chains.

    @since 0.187.7
    @stability Stable *)

let ( let* ) = Result.bind
let ( let+ ) x f = Result.map f x

let both a b =
  match a, b with
  | Ok a_val, Ok b_val -> Ok (a_val, b_val)
  | Error e, _ -> Error e
  | _, Error e -> Error e
;;

let ( and* ) = both
let ( and+ ) = both
