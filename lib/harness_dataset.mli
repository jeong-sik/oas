(** JSONL-backed harness datasets.

    @stability Evolving
    @since 0.93.1 *)

type t = Harness_case.t list

val load : path:string -> (t, Error.sdk_error) result
val save : path:string -> t -> (unit, Error.sdk_error) result
val append_case : path:string -> Harness_case.t -> (unit, Error.sdk_error) result
