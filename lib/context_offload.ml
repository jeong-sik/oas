(** Context offload: move large tool results to the filesystem.

    When a tool result exceeds [threshold_bytes], writes the full
    content to a file and replaces it with a path reference + preview.
    This reduces context window consumption while preserving access
    to the original data.

    Design: thin facade over filesystem I/O. If offload fails,
    the original content is returned unchanged (fail-open).

    @since 0.62.0 *)

(** Offload configuration. *)
type config =
  { threshold_bytes : int (** Content size above which offload triggers *)
  ; output_dir : string (** Directory for offloaded files *)
  ; preview_len : int (** Characters of content to keep as preview *)
  }

let default_config =
  { threshold_bytes = 4096
  ; output_dir = Filename.get_temp_dir_name ()
  ; preview_len = 200
  }
;;

(** Result of an offload attempt. *)
type offload_result =
  | Kept of string (** Content below threshold, unchanged *)
  | Offloaded of
      { path : string (** Filesystem path of full content *)
      ; preview : string (** Truncated preview *)
      ; original_bytes : int (** Size of original content *)
      }

(** Attempt to offload content.

    If [content] length exceeds [config.threshold_bytes], writes it
    to a file in [config.output_dir] and returns [Offloaded].
    Otherwise returns [Kept]. On write failure, returns [Kept]
    (fail-open: prefer larger context over lost data). *)
let maybe_offload ~(config : config) ~(tool_name : string) (content : string)
  : offload_result
  =
  let len = String.length content in
  if len <= config.threshold_bytes
  then Kept content
  else (
    let filename =
      Printf.sprintf
        "oas_offload_%s_%d.txt"
        (String.map (fun c -> if c = '/' || c = ' ' then '_' else c) tool_name)
        (int_of_float (Unix.gettimeofday () *. 1000.0))
    in
    let path = Filename.concat config.output_dir filename in
    match Fs_result.write_file path content with
    | Ok () ->
      let preview =
        if len <= config.preview_len
        then content
        else String.sub content 0 config.preview_len
      in
      Offloaded { path; preview; original_bytes = len }
    | Error _ ->
      (* Fail-open: if we can't write, keep original content *)
      Kept content)
;;

(** Format an offloaded result as a replacement string for the context.

    [Kept] returns the original. [Offloaded] returns a compact reference
    with preview text. *)
let to_context_string = function
  | Kept content -> content
  | Offloaded { path; preview; original_bytes } ->
    Printf.sprintf "[Offloaded to %s (%d bytes)]\n%s..." path original_bytes preview
;;

(** Convenience: offload and immediately format.

    Equivalent to [maybe_offload ... |> to_context_string]. *)
let offload_tool_result ~config ~tool_name content =
  maybe_offload ~config ~tool_name content |> to_context_string
;;
