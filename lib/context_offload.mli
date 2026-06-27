(** Context offload: move large tool results to the filesystem.

    Reduces context window consumption by writing large tool outputs
    to files and replacing them with path references + previews.

    Fail-open design: on write failure, original content is preserved.

    @since 0.62.0

    @stability Evolving
    @since 0.93.1 *)

(** Offload configuration. *)
type config =
  { threshold_bytes : int
  ; output_dir : string
  ; preview_len : int
  }

(** Default offload configuration.
    Resolves [Filename.get_temp_dir_name ()] at call time. *)
val default_config : unit -> config

(** Result of an offload attempt. *)
type offload_result =
  | Kept of string
  | Offloaded of
      { path : string
      ; preview : string
      ; original_bytes : int
      }

(** Attempt to offload content to the filesystem.

    Returns [Offloaded] when content exceeds [config.threshold_bytes],
    [Kept] otherwise or on write failure. Write failures are emitted via
    [Llm_provider.Diag.warn] before preserving the original content. *)
val maybe_offload : config:config -> tool_name:string -> string -> offload_result

(** Format an offload result for context insertion. *)
val to_context_string : offload_result -> string

(** Convenience: offload and format in one call. *)
val offload_tool_result : config:config -> tool_name:string -> string -> string
