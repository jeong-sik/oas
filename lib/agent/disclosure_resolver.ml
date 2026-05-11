(** Per-turn disclosure level resolution.

    Combines an optional [resolver] callback with a [static] fallback
    to produce the effective {!Tool.disclosure_level} for a turn.

    Precedence:
    - [resolver = None]: return [static] as-is.
    - [resolver = Some f]: call [f last_results];
      - [Some override]: return [Some override].
      - [None]: return [static].

    @since 0.195.0 *)

let resolve
      ~(resolver : (Types.tool_result list -> Tool.disclosure_level option) option)
      ~(static : Tool.disclosure_level option)
      ~(last_results : Types.tool_result list)
  : Tool.disclosure_level option
  =
  match resolver with
  | None -> static
  | Some f ->
    (match f last_results with
     | Some _ as override -> override
     | None -> static)
;;
