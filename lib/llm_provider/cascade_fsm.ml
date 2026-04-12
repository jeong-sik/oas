(** Cascade FSM — pure decision logic for multi-provider failover.

    @since 0.120.0 *)

(* ── Types ──────────────────────────────────────── *)

type provider_outcome =
  | Call_ok of Types.api_response
  | Call_err of Http_client.http_error
  | Accept_rejected of { response : Types.api_response; reason : string }
  | Slot_full

type decision =
  | Accept of Types.api_response
  | Accept_on_exhaustion of { response : Types.api_response; reason : string }
  | Try_next of { last_err : Http_client.http_error option }
  | Exhausted of { last_err : Http_client.http_error option }

(* ── Decision function ──────────────────────────── *)

let decide ~accept_on_exhaustion ~is_last outcome =
  match outcome with
  | Call_ok resp ->
    Accept resp
  | Slot_full ->
    Try_next { last_err = Some (Http_client.NetworkError {
        message = "slot full, cascading to next provider" }) }
  | Accept_rejected { response; reason } ->
    if is_last && accept_on_exhaustion then
      Accept_on_exhaustion { response; reason }
    else if is_last then
      Exhausted { last_err = Some (Http_client.AcceptRejected { reason }) }
    else
      Try_next { last_err = Some (Http_client.AcceptRejected { reason }) }
  | Call_err err ->
    let should_cascade = Cascade_health_filter.should_cascade_to_next err in
    if should_cascade then
      Try_next { last_err = Some err }
    else
      Exhausted { last_err = Some err }

(* ── Error formatting ───────────────────────────── *)

let format_exhausted_error last_err =
  let msg = match last_err with
    | Some (Http_client.HttpError { code; body }) ->
      Printf.sprintf "HTTP %d: %s" code
        (if String.length body > Constants.Truncation.max_error_body_length
         then String.sub body 0 Constants.Truncation.max_error_body_length ^ "..."
         else body)
    | Some (Http_client.AcceptRejected { reason }) -> reason
    | Some (Http_client.NetworkError { message }) -> message
    | None -> "No providers available"
  in
  match last_err with
  | Some (Http_client.AcceptRejected _ as err) -> err
  | _ ->
    Http_client.NetworkError {
      message = Printf.sprintf "All models failed: %s" msg
    }

(* ── Inline tests ───────────────────────────────── *)

[@@@coverage off]

let make_resp model =
  Types.{ id = "r1"; model; stop_reason = EndTurn;
          content = [Text "ok"]; usage = None; telemetry = None }

(* --- Call_ok always accepts --- *)

let%test "decide: Call_ok -> Accept" =
  match decide ~accept_on_exhaustion:false ~is_last:false
          (Call_ok (make_resp "m1")) with
  | Accept _ -> true | _ -> false

let%test "decide: Call_ok last -> Accept" =
  match decide ~accept_on_exhaustion:true ~is_last:true
          (Call_ok (make_resp "m1")) with
  | Accept _ -> true | _ -> false

(* --- Slot_full always tries next --- *)

let%test "decide: Slot_full -> Try_next" =
  match decide ~accept_on_exhaustion:false ~is_last:false Slot_full with
  | Try_next _ -> true | _ -> false

let%test "decide: Slot_full last -> Try_next" =
  match decide ~accept_on_exhaustion:false ~is_last:true Slot_full with
  | Try_next _ -> true | _ -> false

(* --- Accept_rejected non-last -> Try_next --- *)

let%test "decide: Accept_rejected non-last -> Try_next" =
  match decide ~accept_on_exhaustion:false ~is_last:false
          (Accept_rejected { response = make_resp "m1"; reason = "bad json" }) with
  | Try_next { last_err = Some (Http_client.AcceptRejected _) } -> true
  | _ -> false

(* --- Accept_rejected last + accept_on_exhaustion -> Accept_on_exhaustion --- *)

let%test "decide: Accept_rejected last exhaustion -> Accept_on_exhaustion" =
  match decide ~accept_on_exhaustion:true ~is_last:true
          (Accept_rejected { response = make_resp "m1"; reason = "bad json" }) with
  | Accept_on_exhaustion { reason = "bad json"; _ } -> true
  | _ -> false

(* --- Accept_rejected last + no exhaustion -> Exhausted --- *)

let%test "decide: Accept_rejected last no_exhaustion -> Exhausted" =
  match decide ~accept_on_exhaustion:false ~is_last:true
          (Accept_rejected { response = make_resp "m1"; reason = "bad json" }) with
  | Exhausted { last_err = Some (Http_client.AcceptRejected { reason = "bad json" }) } -> true
  | _ -> false

(* --- Call_err cascadeable -> Try_next --- *)

let%test "decide: Call_err 500 -> Try_next" =
  match decide ~accept_on_exhaustion:false ~is_last:false
          (Call_err (Http_client.HttpError { code = 500; body = "server error" })) with
  | Try_next _ -> true | _ -> false

let%test "decide: Call_err 429 -> Try_next" =
  match decide ~accept_on_exhaustion:false ~is_last:false
          (Call_err (Http_client.HttpError { code = 429; body = "" })) with
  | Try_next _ -> true | _ -> false

let%test "decide: Call_err network -> Try_next" =
  match decide ~accept_on_exhaustion:false ~is_last:false
          (Call_err (Http_client.NetworkError { message = "refused" })) with
  | Try_next _ -> true | _ -> false

(* --- Call_err non-cascadeable -> Exhausted --- *)

let%test "decide: Call_err 404 -> Exhausted" =
  match decide ~accept_on_exhaustion:false ~is_last:false
          (Call_err (Http_client.HttpError { code = 404; body = "" })) with
  | Exhausted _ -> true | _ -> false

let%test "decide: Call_err generic 400 -> Exhausted" =
  match decide ~accept_on_exhaustion:false ~is_last:false
          (Call_err (Http_client.HttpError { code = 400; body = "" })) with
  | Exhausted _ -> true | _ -> false

(* --- Call_err 400 ollama parse -> Try_next (via cascade_health_filter) --- *)

let%test "decide: Call_err 400 ollama parse -> Try_next" =
  match decide ~accept_on_exhaustion:false ~is_last:false
          (Call_err (Http_client.HttpError {
             code = 400;
             body = {|{"error":"can't find closing '}' symbol"}|} })) with
  | Try_next _ -> true | _ -> false

(* --- Call_err 400 context overflow -> Try_next --- *)

let%test "decide: Call_err 400 overflow -> Try_next" =
  match decide ~accept_on_exhaustion:false ~is_last:false
          (Call_err (Http_client.HttpError {
             code = 400;
             body = {|{"error":"exceeds the available context size (8192 tokens)"}|} })) with
  | Try_next _ -> true | _ -> false

(* --- Call_err resource exhaustion -> Exhausted --- *)

let%test "decide: Call_err EMFILE -> Exhausted" =
  match decide ~accept_on_exhaustion:false ~is_last:false
          (Call_err (Http_client.NetworkError {
             message = "Unix.Unix_error(Unix.EMFILE, \"socket\", \"\")" })) with
  | Exhausted _ -> true | _ -> false

(* --- format_exhausted_error --- *)

let%test "format_exhausted_error None" =
  match format_exhausted_error None with
  | Http_client.NetworkError { message } ->
    String.length message > 0
  | _ -> false

let%test "format_exhausted_error AcceptRejected preserved" =
  match format_exhausted_error
          (Some (Http_client.AcceptRejected { reason = "bad" })) with
  | Http_client.AcceptRejected { reason = "bad" } -> true
  | _ -> false

let%test "format_exhausted_error HttpError wrapped" =
  match format_exhausted_error
          (Some (Http_client.HttpError { code = 500; body = "fail" })) with
  | Http_client.NetworkError { message } ->
    String.length message > 0
  | _ -> false
