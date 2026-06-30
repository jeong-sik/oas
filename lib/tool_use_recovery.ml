(** Tool use recovery — strictly extract ToolUse blocks from text content when the
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

    This fallback deliberately does not repair malformed JSON or scrape JSON
    out of surrounding prose. It may strip a Markdown code fence wrapper, but
    promotion requires the entire resulting text to be exactly one balanced
    JSON object that parses with {!Yojson.Safe.from_string}. Ambiguous text,
    extra prose, or repair-needed JSON remains Text.

    @since 0.136.0 *)

open Types

let _log = Log.create ~module_name:"tool_use_recovery" ()

(* ── JSON candidate location ─────────────────────────────── *)

(** Strip a Markdown code fence wrapper without changing the wrapped JSON. *)
let strip_markdown_fence (s : string) =
  let trimmed = String.trim s in
  let fence = "```" in
  let fence_len = String.length fence in
  if String.length trimmed < fence_len || String.sub trimmed 0 fence_len <> fence
  then trimmed
  else (
    let lines = String.split_on_char '\n' trimmed in
    match lines with
    | _opening :: rest ->
      (match List.rev rest with
       | closing :: body_rev when String.trim closing = fence ->
         String.trim (String.concat "\n" (List.rev body_rev))
       | _ -> trimmed)
    | _ -> trimmed)
;;

(** Find the first balanced top-level JSON object in a string by
    scanning for '{' and tracking depth while respecting string
    literals. Returns [Some (start, length)] or [None]. *)
let find_json_objects (s : string) : (int * int) list =
  let len = String.length s in
  let rec find_start i =
    if i >= len then None else if s.[i] = '{' then Some i else find_start (i + 1)
  in
  let rec loop i acc =
    match find_start i with
    | None -> List.rev acc
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
      if !end_idx >= 0
      then loop (!end_idx + 1) ((start, !end_idx - start + 1) :: acc)
      else List.rev acc
  in
  loop 0 []
;;

let find_json_object (s : string) : (int * int) option =
  match find_json_objects s with
  | first :: _ -> Some first
  | [] -> None
;;

(** Try to parse a JSON object from a string using strict JSON parsing after
    locating exactly one balanced object that spans the whole text. Returns
    [None] when no object can be located, when prose surrounds the object, when
    more than one candidate exists, or when parsing fails. *)
let try_parse_json_object (s : string) : Yojson.Safe.t option =
  let stripped = strip_markdown_fence s |> String.trim in
  match find_json_objects stripped with
  | [ (start, length) ] when start = 0 && length = String.length stripped ->
    let candidate = String.sub stripped start length in
    (match Yojson.Safe.from_string candidate with
     | `Assoc _ as parsed -> Some parsed
     | _ -> None
     | exception Yojson.Json_error _ -> None)
  | [] | [ _ ] | _ :: _ :: _ -> None
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
              (* Double-stringified: arguments is a strict JSON-encoded string. *)
              (match Yojson.Safe.from_string s with
               | parsed -> Some parsed
               | exception Yojson.Json_error _ -> None)
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
