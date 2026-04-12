(** JSON config loading with mtime-based hot-reload.

    @since 0.59.0
    @since 0.92.0 extracted from Cascade_config

    @stability Internal
    @since 0.93.1 *)

(** Load and cache a raw JSON config file.
    Cached with mtime-based hot-reload. *)
val load_json : string -> (Yojson.Safe.t, string) result

(** Load a named model list from a JSON config file.

    The JSON file maps ["{name}_models"] keys to string arrays.
    Results are cached and hot-reloaded when the file mtime changes.
    Returns an empty list when the file is missing or the key is absent. *)
val load_profile :
  config_path:string ->
  name:string ->
  string list

(** Per-cascade inference parameter overrides. *)
type inference_params = {
  temperature: float option;
  max_tokens: int option;
}

(** Resolve inference parameters from cascade.json.

    Resolution order:
    1. ["{name}_temperature"] / ["{name}_max_tokens"]
    2. ["default_temperature"] / ["default_max_tokens"]
    3. [None] (caller uses own defaults) *)
val resolve_inference_params :
  config_path:string -> name:string -> inference_params

(** Resolve per-cascade API key env var overrides from cascade.json.

    Resolution order:
    1. ["{name}_api_key_env"] from [config_path]
    2. ["default_api_key_env"] from [config_path]
    3. Empty list (use provider registry defaults)

    The JSON value can be a string (applies to all providers via ["*"] key)
    or an object mapping provider names to env var names.

    @since 0.122.0 *)
val resolve_api_key_env :
  config_path:string -> name:string -> (string * string) list
