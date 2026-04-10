(** Z.AI catalog and routing helpers.

    Centralizes official endpoint constants, model aliases, and
    conservative concurrency defaults for z.ai integrations.

    @stability Internal *)

type api_mode =
  | General_api
  | Coding_plan

val general_base_url : string
val coding_base_url : string
val anthropic_base_url : string

val is_glm_model_id : string -> bool
val is_zai_base_url : string -> bool

val is_coding_base_url : string -> bool
val is_anthropic_base_url : string -> bool
val mode_of_base_url : string -> api_mode

val glm_auto_models : unit -> string list
val glm_coding_auto_models : unit -> string list

val resolve_glm_alias : default_model:string -> string -> string
val resolve_glm_coding_alias : default_model:string -> string -> string

val general_concurrency_for_model : string -> int
val coding_concurrency_default : int

val throttle_key_for_chat : base_url:string -> model_id:string -> string
