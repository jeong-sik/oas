(** SSE (Server-Sent Events) line-level parser.

    Extracted from streaming.ml for reuse by mcp_http.ml and consumers.
    Follows the SSE spec: event/data/id/retry fields, blank line = dispatch.

    Usage:
    {[
      Sse_parser.parse_lines buf_reader ~on_event:(fun evt ->
        match evt.data with
        | "[DONE]" -> ()
        | data -> process data)
    ]}
*)

type raw_sse_event =
  { event_type : string option
  ; data : string
  ; id : string option
  ; retry : int option
  }

(** Parse SSE lines from a [Buf_read.t] and call [on_event] for each
    complete event (delimited by a blank line).
    Returns when the stream ends ([End_of_file]). *)
let parse_lines (reader : Eio.Buf_read.t) ~on_event =
  let current_event_type = ref None in
  let current_data = Buffer.create 256 in
  let current_id = ref None in
  let current_retry = ref None in
  let dispatch () =
    let data = Buffer.contents current_data in
    if data <> ""
    then (
      let evt =
        { event_type = !current_event_type
        ; data
        ; id = !current_id
        ; retry = !current_retry
        }
      in
      on_event evt);
    current_event_type := None;
    Buffer.clear current_data;
    current_id := None;
    current_retry := None
  in
  let process_line line =
    if String.length line = 0
    then dispatch ()
    else if String.length line > 0 && line.[0] = ':'
    then () (* comment -- ignore *)
    else (
      let field, value =
        match String.index_opt line ':' with
        | Some pos ->
          let f = String.sub line 0 pos in
          let v_start =
            if pos + 1 < String.length line && line.[pos + 1] = ' '
            then pos + 2
            else pos + 1
          in
          f, String.sub line v_start (String.length line - v_start)
        | None -> line, ""
      in
      match field with
      | "event" -> current_event_type := Some value
      | "data" ->
        if Buffer.length current_data > 0 then Buffer.add_char current_data '\n';
        Buffer.add_string current_data value
      | "id" -> current_id := Some value
      | "retry" ->
        (match int_of_string_opt value with
         | Some n -> current_retry := Some n
         | None -> ())
      | _ -> () (* unknown field -- ignore *))
  in
  let rec read_lines () =
    match Eio.Buf_read.line reader with
    | line ->
      process_line line;
      read_lines ()
    | exception End_of_file ->
      (* Dispatch any remaining buffered data *)
      dispatch ()
  in
  read_lines ()
;;
