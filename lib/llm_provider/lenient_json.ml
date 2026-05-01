open Base
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
let strip_markdown_fence (s : string) : string =
  let s = String.trim s in
  let len = String.length s in
  if len < 6
  then s
  else (
    (* Check if starts with ``` *)
    let starts_with_fence = len >= 3 && s.[0] = '`' && s.[1] = '`' && s.[2] = '`' in
    if not starts_with_fence
    then s
    else (
      (* Find end of first line (skip language tag) *)
      let first_newline =
        match String.index_opt s '\n' with
        | Some i -> i
        | None -> len
      in
      (* Check if ends with ``` *)
      let ends_with_fence =
        len >= 3 && s.[len - 1] = '`' && s.[len - 2] = '`' && s.[len - 3] = '`'
      in
      if not ends_with_fence
      then s
      else (
        let inner_start = first_newline + 1 in
        let inner_end = len - 3 in
        if inner_start >= inner_end
        then s
        else String.trim (String.sub s inner_start (inner_end - inner_start)))))
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
        && (inner_trimmed.[0] = '{' || inner_trimmed.[0] = '[')
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
    let s1 = strip_markdown_fence s in
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
