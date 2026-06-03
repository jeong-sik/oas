(** Canonical tool-call projection (RFC-OAS-024, WP8 — lane A).

    Implementation notes:
    - Pure, total functions over [Types.content_block] / [Types.api_response].
    - No [id_origin] (lane A): [call_id] is the block's id verbatim, which is
      already the native wire id or a synthesized id depending on the provider
      parse path. We do not re-synthesize or re-classify ids here.
    - Reasoning attribution is a positional heuristic (RFC-OAS-024 R1): the
      reasoning block immediately preceding a tool-use block, within the same
      response, is attributed to that call. When none precedes it, the call is
      [No_reasoning] (or [Suppressed] when request config disabled reasoning).
      The 3-way variant keeps "no reasoning" honest rather than fabricated. *)

type reasoning_kind =
  | Thinking
  | Redacted_thinking
  | Reasoning_content

type reasoning_state =
  { kind : reasoning_kind
  ; content : string
  ; tokens : int option
  }

type reasoning_link =
  | No_reasoning
  | Suppressed
  | Available of reasoning_state

type provider_tool_call =
  { call_id : string
  ; provider_kind : Provider_kind.t
  ; name : string
  ; arguments : Yojson.Safe.t
  ; order_index : int
  ; reasoning : reasoning_link
  }

type provider_tool_result =
  { call_id : string
  ; content : string
  ; content_blocks : Types.content_block list option
  ; structured_content : Yojson.Safe.t option
  ; is_error : bool
  }

let tool_result_of_block (block : Types.content_block) : provider_tool_result option =
  match block with
  | Types.ToolResult { tool_use_id; content; is_error; json; content_blocks } ->
    Some
      { call_id = tool_use_id
      ; content
      ; content_blocks
      ; structured_content = json
      ; is_error
      }
  | Types.Text _
  | Types.Thinking _
  | Types.RedactedThinking _
  | Types.ToolUse _
  | Types.Image _
  | Types.Document _
  | Types.Audio _ -> None
;;

(* Reasoning carried by a content block that immediately precedes a tool call.
   [reasoning_tokens] (response-level) is attached when the block is the kind
   that the provider counts as reasoning. *)
let reasoning_state_of_block ~reasoning_tokens (block : Types.content_block)
  : reasoning_state option
  =
  match block with
  | Types.Thinking { content; _ } ->
    Some { kind = Thinking; content; tokens = reasoning_tokens }
  | Types.RedactedThinking content ->
    Some { kind = Redacted_thinking; content; tokens = reasoning_tokens }
  | Types.Text _
  | Types.ToolUse _
  | Types.ToolResult _
  | Types.Image _
  | Types.Document _
  | Types.Audio _ -> None
;;

let tool_calls_of_response
      ~(provider_kind : Provider_kind.t)
      ~(reasoning_suppressed : bool)
      (response : Types.api_response)
  : provider_tool_call list
  =
  let reasoning_tokens =
    Option.bind response.telemetry (fun t -> t.Types.reasoning_tokens)
  in
  (* Walk the block list once, remembering the most recent reasoning block so
     it can be attributed to the next tool call. [order_index] counts only
     tool-use blocks (RFC-OAS-024 D3). *)
  let _, _, rev_calls =
    List.fold_left
      (fun (order_index, pending_reasoning, acc) (block : Types.content_block) ->
         match block with
         | Types.ToolUse { id; name; input } ->
           let reasoning =
             match pending_reasoning with
             | Some state -> Available state
             | None -> if reasoning_suppressed then Suppressed else No_reasoning
           in
           let call =
             { call_id = id
             ; provider_kind
             ; name
             ; arguments = input
             ; order_index
             ; reasoning
             }
           in
           order_index + 1, None, call :: acc
         | other ->
           let pending_reasoning =
             match reasoning_state_of_block ~reasoning_tokens other with
             | Some _ as state -> state
             | None -> pending_reasoning
           in
           order_index, pending_reasoning, acc)
      (0, None, [])
      response.content
  in
  List.rev rev_calls
;;
