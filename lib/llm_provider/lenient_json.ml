(** Lenient JSON parser — recovers common LLM output malformations.

    LLMs frequently produce malformed JSON: markdown fences, trailing
    commas, unclosed brackets, double-stringified values, unquoted keys.
    This module applies a deterministic chain of recovery transforms
    before falling back to raw wrapping.

    Transform pipeline (order matters):
    1. Strip markdown code fences
    2. Detect and unwrap double-stringified JSON
    3. Strip trailing commas
    4. Complete incomplete boolean/null keywords
    5. Close unclosed brackets
    6. Standard parse

    Each transform is pure and deterministic — no heuristics.

    @since 0.100.0 *)

(** {1 Individual Transforms} *)

(** Strip markdown code fences: [```json ... ```] -> inner content.
    Handles [```json], [```JSON], [```], with optional language tag. *)
let find_substring s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec loop i =
    if i + len_sub > len_s then None
    else if String.sub s i len_sub = sub then Some i
    else loop (i + 1)
  in
  loop 0

let rfind_substring s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec loop i =
    if i < 0 then None
    else if String.sub s i len_sub = sub then Some i
    else loop (i - 1)
  in
  loop (len_s - len_sub)

let strip_markdown_fence (s : string) : string =
  let s = String.trim s in
  match find_substring s "```", rfind_substring s "```" with
  | Some start_idx, Some end_idx when start_idx < end_idx ->
      let rec find_nl i =
        if i >= end_idx then start_idx + 3
        else if s.[i] = '
' then i + 1
        else find_nl (i + 1)
      in
      let inner_start = find_nl (start_idx + 3) in
      String.trim (String.sub s inner_start (end_idx - inner_start))
  | _ -> s
;;

let strip_conversational_prefix (s : string) : string =
  let s = String.trim s in
  let first_brace = try String.index s '{' with Not_found -> String.length s in
  let first_bracket = try String.index s '[' with Not_found -> String.length s in
  let first = min first_brace first_bracket in
  if first = String.length s then s
  else
    let last_brace = try String.rindex s '}' with Not_found -> -1 in
    let last_bracket = try String.rindex s ']' with Not_found -> -1 in
    let last = max last_brace last_bracket in
    if last > first then
      String.trim (String.sub s first (last - first + 1))
    else
      String.trim (String.sub s first (String.length s - first))
;;

(** Detect and unwrap double-stringified JSON.
    When an LLM returns ["{\"key\": \"val\"}"] (a JSON string containing
    escaped JSON), parse the outer string then parse the inner value.
    Only triggers when the value is a string that itself parses as JSON
    object or array. *)
let unwrap_double_stringify (s : string) : string =
  let trimmed = String.trim s in
  let len = String.length trimmed in
  if len < 4
  then s
  else if trimmed.[0] = '"' && trimmed.[len - 1] = '"'
  then (
    (* Looks like a JSON string literal — try parsing to get inner string *)
    match Yojson.Safe.from_string trimmed with
    | `String inner ->
      (* Check if inner string is itself valid JSON object/array *)
      let inner_trimmed = String.trim inner in
      if
        String.length inner_trimmed > 0
        && (inner_trimmed.[0] = '{' || inner_trimmed.[0] = '[' || inner_trimmed.[0] = '"')
      then (
        match Yojson.Safe.from_string inner_trimmed with
        | _ -> inner_trimmed (* inner parses: use it *)
        | exception Yojson.Json_error _ -> s)
      else s
    | _ -> s
    | exception Yojson.Json_error _ -> s)
  else s
;;

(** Remove trailing commas before closing brackets.
    Handles [{"a": 1,}], [{"a": 1, }], [[1, 2,]]. *)
let strip_trailing_commas (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let in_string = ref false in
  let escape = ref false in
  let len = String.length s in
  for i = 0 to len - 1 do
    let c = s.[i] in
    if !escape
    then (
      Buffer.add_char buf c;
      escape := false)
    else if c = '\\' && !in_string
    then (
      Buffer.add_char buf c;
      escape := true)
    else if c = '"'
    then (
      in_string := not !in_string;
      Buffer.add_char buf c)
    else if c = ',' && not !in_string
    then (
      (* Look ahead: skip whitespace, check if next non-ws char is ] or } *)
      let j = ref (i + 1) in
      while
        !j < len && (s.[!j] = ' ' || s.[!j] = '\t' || s.[!j] = '\n' || s.[!j] = '\r')
      do
        incr j
      done;
      if !j < len && (s.[!j] = '}' || s.[!j] = ']')
      then () (* skip the trailing comma *)
      else Buffer.add_char buf c)
    else Buffer.add_char buf c
  done;
  Buffer.contents buf
;;

(** Complete incomplete boolean/null keywords at end of input.
    Handles truncated output like [{"flag": tru] -> [{"flag": true].
    Only completes unambiguous prefixes. *)
let complete_keywords (s : string) : string =
  let trimmed = String.trim s in
  let len = String.length trimmed in
  if len < 2
  then s
  else (
    (* Find the last non-whitespace, non-bracket content *)
    let completions =
      [ "tru", "true"; "fals", "false"; "fal", "false"; "nul", "null"; "nu", "null" ]
    in
    let result = ref s in
    List.iter
      (fun (prefix, full) ->
         let plen = String.length prefix in
         if
           plen <= len
           && String.sub trimmed (len - plen) plen = prefix
           && (len = plen
               ||
               let c = trimmed.[len - plen - 1] in
               c = ':' || c = ' ' || c = '\t' || c = ',' || c = '[')
         then result := String.sub trimmed 0 (len - plen) ^ full)
      completions;
    !result)
;;

(** Close unclosed brackets/braces.
    Counts unmatched [{] and [[] outside strings, appends matching closers. *)
let close_brackets (s : string) : string =
  let stack = Stack.create () in
  let in_string = ref false in
  let escape = ref false in
  String.iter
    (fun c ->
       if !escape
       then escape := false
       else if c = '\\' && !in_string
       then escape := true
       else if c = '"'
       then in_string := not !in_string
       else if not !in_string
       then
         if c = '{'
         then Stack.push '}' stack
         else if c = '['
         then Stack.push ']' stack
         else if (c = '}' || c = ']') && not (Stack.is_empty stack)
         then ignore (Stack.pop stack))
    s;
  if Stack.is_empty stack
  then s
  else (
    let buf = Buffer.create (String.length s + Stack.length stack) in
    Buffer.add_string buf s;
    Stack.iter (fun c -> Buffer.add_char buf c) stack;
    Buffer.contents buf)
;;

(** {1 Parse Pipeline} *)

(** Try to parse a string as JSON, applying recovery transforms on failure.
    Returns the parsed JSON value, or [`Assoc [("raw", `String s)]]
    if all recovery attempts fail.

    The transform chain is:
    1. Direct parse (fast path)
    2. Strip markdown fences -> parse
    3. Unwrap double-stringify -> parse
    4. Strip trailing commas -> parse
    5. Complete keywords -> close brackets -> parse
    6. All transforms combined -> parse
    7. Fallback: wrap as raw string *)
let parse (s : string) : Yojson.Safe.t =
  let try_parse str =
    match Yojson.Safe.from_string str with
    | json -> Some json
    | exception Yojson.Json_error _ -> None
  in
  (* Unwrap double-stringified JSON: if direct parse yields a string
     whose content is itself a JSON object or array, parse the inner value.
     This handles LLMs that wrap JSON in an extra layer of string escaping. *)
  let maybe_unwrap_string = function
    | `String inner ->
      let t = String.trim inner in
      if String.length t > 0 && (t.[0] = '{' || t.[0] = '[')
      then (
        match try_parse t with
        | Some json -> json
        | None -> `String inner)
      else `String inner
    | json -> json
  in
  (* 1. Direct parse *)
  match try_parse s with
  | Some json -> maybe_unwrap_string json
  | None ->
    (* 2. Strip markdown fences *)
    let s1 = strip_conversational_prefix (strip_markdown_fence s) in
    (match try_parse s1 with
     | Some json -> json
     | None ->
       (* 3. Unwrap double-stringify *)
       let s2 = unwrap_double_stringify s1 in
       (match try_parse s2 with
        | Some json -> json
        | None ->
          (* 4. Strip trailing commas *)
          let s3 = strip_trailing_commas s2 in
          (match try_parse s3 with
           | Some json -> json
           | None ->
             (* 5. Complete keywords + close brackets *)
             let s4 = close_brackets (complete_keywords s3) in
             (match try_parse s4 with
              | Some json -> json
              | None ->
                (* 6. All transforms combined on original.
        close_brackets first so trailing comma before closer is visible,
        then strip_trailing_commas to clean up. *)
                let s5 =
                  s
                  |> strip_markdown_fence
                  |> strip_conversational_prefix
                  |> unwrap_double_stringify
                  |> complete_keywords
                  |> close_brackets
                  |> strip_trailing_commas
                in
                (match try_parse s5 with
                 | Some json -> json
                 | None ->
                   (* 7. Fallback *)
                   `Assoc [ "raw", `String s ])))))
;;
