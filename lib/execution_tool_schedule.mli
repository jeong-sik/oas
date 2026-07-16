(** Canonical validation and durable codec for execution tool schedules. *)

val validate : Hooks.tool_schedule -> (unit, string) result
val equal : Hooks.tool_schedule -> Hooks.tool_schedule -> bool
val to_yojson : Hooks.tool_schedule -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (Hooks.tool_schedule, string) result
