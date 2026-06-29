(** Tool use recovery — extract ToolUse blocks from text content when the
    provider returns a tool-call intent as text instead of a proper
    ToolUse block.

    Motivated by Glm / Ollama providers that receive [tool_choice]
    parameters but emit the call as JSON inside a Text block (often
    wrapped in Markdown fences). Without recovery, downstream tool
    execution never fires because the call is buried in text rather than
    surfaced as a ToolUse block, even though the model's intent was
    correct.

    Pure module: takes a response and a list of valid tool names,
    returns either the original response (unchanged) or a response with
    Text blocks promoted to ToolUse blocks.

    Reference: Samchon function calling harness, Layer 1 "Lenient parse"
    (dev.to/samchon, DashScope Meetup 2025). Delegates JSON normalization to
    {!Llm_provider.Lenient_json} (markdown fence strip, double-stringify
    unwrap, trailing comma cleanup, bracket completion).

    @since 0.136.0 *)

open Types

let _log = Log.create ~module_name:"tool_use_recovery" ()

(* ── JSON candidate location ─────────────────────────────── *)

(** Find the first balanced top-level JSON object in a string by
    scanning for '{' and tracking depth while respecting string
    literals. Returns [Some (start, length)] or [None]. *)
let find_json_object (s : string) : (int * int) option =
  let len = String.length s in
  let rec find_start i =
    if i >= len then None else if s.[i] = '{' then Some i else find_start (i + 1)
  in
  match find_start 0 with
  | None -> None
  | Some start ->
    let depth = ref 0 in
    let in_string = ref false in
    let escaped = ref false in
    let end_idx = ref (-1) in
    let i = ref start in
    while !end_idx = -1 && !i < len do
      let c = s.[!i] in
      if !escaped
      then escaped := false
      else if c = '\\' && !in_string
      then escaped := true
      else if c = '"'
      then in_string := not !in_string
      else if not !in_string
      then
        if c = '{'
        then incr depth
        else if c = '}'
        then (
          decr depth;
          if !depth = 0 then end_idx := !i);
      incr i
    done;
    if !end_idx >= 0 then Some (start, !end_idx - start + 1) else None
;;

(** Try to parse a JSON object from a string using {!Lenient_json.parse}
    after locating a balanced object. Returns [None] only when no
    object can be located at all (Lenient_json never raises). *)
let try_parse_json_object (s : string) : Yojson.Safe.t option =
  (* Lenient_json handles markdown fences, double-stringify, trailing
     commas, and unclosed brackets. We still locate the first balanced
     object first, because content blocks may wrap JSON in surrounding
     prose ("I will call: {...}"). *)
  let stripped = Llm_provider.Lenient_json.strip_markdown_fence s in
  match find_json_object stripped with
  | None ->
    (* Fall back to lenient_json on the whole input — handles cases
       where the stream is the JSON itself, possibly truncated. *)
    let parsed = Llm_provider.Lenient_json.parse stripped in
    (match parsed with
     | `Assoc [ ("raw", `String _) ] -> None (* Sentinel for parse failure *)
     | other -> Some other)
  | Some (start, length) ->
    let candidate = String.sub stripped start length in
    let parsed = Llm_provider.Lenient_json.parse candidate in
    (match parsed with
     | `Assoc [ ("raw", `String _) ] -> None
     | other -> Some other)
;;

(* ── Tool call shape matching ────────────────────────────── *)

(** Extract [(name, input)] from a JSON value matching one of the
    common tool-call shapes:

    - Anthropic-style: [{"name": "X", "input": {...}}]
    - OpenAI function call: [{"name": "X", "arguments": {...}}]
      where arguments may be a JSON-encoded string (double-stringified)
    - OpenAI tool_calls wrapper: [{"tool_calls": [{"function": {...}}]}]
    - Bare function wrapper: [{"function": {"name": ..., ...}}]

    Returns [None] if no recognizable shape is found. *)
let rec extract_name_and_input (json : Yojson.Safe.t) : (string * Yojson.Safe.t) option =
  match json with
  | `Assoc fields ->
    let name_opt =
      match List.assoc_opt "name" fields with
      | Some (`String s) -> Some s
      | _ -> None
    in
    (match name_opt with
     | Some name ->
       let input_opt =
         match List.assoc_opt "input" fields with
         | Some v -> Some v
         | None ->
           (match List.assoc_opt "arguments" fields with
            | Some (`String s) ->
              (* Double-stringified: arguments is a JSON-encoded string *)
              let inner = Llm_provider.Lenient_json.parse s in
              (match inner with
               | `Assoc [ ("raw", `String _) ] -> Some (`String s)
               | other -> Some other)
            | Some v -> Some v
            | None ->
              (match List.assoc_opt "parameters" fields with
               | Some v -> Some v
               | None -> None))
       in
       Option.map (fun input -> name, input) input_opt
     | None ->
       (match List.assoc_opt "tool_calls" fields with
        | Some (`List (first :: _)) ->
          (match first with
           | `Assoc subfields ->
             (match List.assoc_opt "function" subfields with
              | Some inner -> extract_name_and_input inner
              | None -> extract_name_and_input first)
           | _ -> None)
        | _ ->
          (match List.assoc_opt "function" fields with
           | Some inner -> extract_name_and_input inner
           | None -> None)))
  | _ -> None
;;

(* ── Tool ID generation ──────────────────────────────────── *)

(** Deterministic id for a recovered ToolUse block, derived from its position
    in the content list and a content hash of the tool name and arguments. No
    wall-clock and no mutable counter, so the same response recovers to the
    same ids on every run — required because this module documents itself
    "Pure" (RFC-OAS-029 S10.1). The block index keeps ids distinct even when
    two recovered calls carry identical name and arguments. *)
let recovery_id ~block_index ~name ~(input : Yojson.Safe.t) =
  let digest =
    Digest.to_hex (Digest.string (name ^ "\000" ^ Yojson.Safe.to_string input))
  in
  Printf.sprintf "recovered_%d_%s" block_index (String.sub digest 0 12)
;;

(* ── Response-level recovery ─────────────────────────────── *)

(** Scan content blocks; replace recoverable Text blocks with ToolUse.
    Returns [(new_content, recovered_count)]. *)
let recover_content_blocks
      ~(valid_tool_names : string list)
      (content : content_block list)
  : content_block list * int
  =
  let recovered = ref 0 in
  let new_content =
    List.mapi
      (fun block_index block ->
         match block with
         | Text text ->
           (match try_parse_json_object text with
            | None -> block
            | Some json ->
              (match extract_name_and_input json with
               | None -> block
               | Some (name, input) when List.mem name valid_tool_names ->
                 incr recovered;
                 ToolUse { id = recovery_id ~block_index ~name ~input; name; input }
               | Some _ -> block))
         | _ -> block)
      content
  in
  new_content, !recovered
;;

(** Top-level recovery. Promotes Text-embedded tool calls to ToolUse
    blocks only when:
    - [valid_tool_names] is non-empty,
    - the response has no ToolUse blocks,
    - at least one Text block matches a recognized tool-call shape
      whose [name] is in [valid_tool_names].

    Otherwise returns the response unchanged. Adjusts [stop_reason]
    from [EndTurn] to [StopToolUse] when recovery succeeds. *)
let recover_response ~(valid_tool_names : string list) (response : api_response)
  : api_response
  =
  if valid_tool_names = []
  then response
  else (
    let has_tool_use =
      List.exists
        (fun (block : Types.content_block) ->
           match block with
           | ToolUse _ -> true
           | Text _
           | Thinking _
           | RedactedThinking _
           | ToolResult _
           | Image _
           | Document _
           | Audio _ -> false)
        response.content
    in
    if has_tool_use
    then response
    else (
      let new_content, count =
        recover_content_blocks ~valid_tool_names response.content
      in
      if count = 0
      then response
      else (
        Log.info
          _log
          "recovered tool use(s) from text content"
          [ Log.I ("count", count); Log.S ("model", response.model) ];
        { response with
          content = new_content
        ; stop_reason =
            (match response.stop_reason with
             | EndTurn -> StopToolUse
             | other -> other)
        })))
;;
