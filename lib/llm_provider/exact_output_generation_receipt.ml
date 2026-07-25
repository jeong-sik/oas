module Exec = Exact_output_execution
module Trace = Exact_output_provider_trace

type call_id = Call_id of string
type provider_trace = Trace.t

type terminal_state =
  { status : int
  ; provider_trace : provider_trace option
  }

(* A response-received receipt carries a provider trace for the same reason a
   terminal one does: the trace is evidence about the response that arrived, and an
   error can be raised after the response without the attempt reaching Terminal.
   test_exact_output_single_surface's "response-received error evidence" case pins
   that shape — status Some 200, a typed cause, and a present trace — and it could
   not hold while this state had nowhere to put one. *)
type response_received_state =
  { status : int option
  ; provider_trace : provider_trace option
  }

type state =
  | Not_started_state
  | Before_dispatch_state
  | Dispatch_started_state
  | Response_received_state of response_received_state
  | Terminal_state of terminal_state

type effect_phase =
  | Not_started
  | Before_dispatch
  | Dispatch_started
  | Response_received
  | Terminal

type t =
  { state : state Atomic.t
  ; call_id : call_id
  ; plan_fingerprint : string
  ; request_body_sha256 : string
  ; catalog_generation : Exact_output_resolver.catalog_generation
  ; catalog_evidence : Exact_output_resolver.catalog_evidence
  ; target_identity : Exact_output_resolver.target_identity
  }

type snapshot =
  { phase : effect_phase
  ; dispatch_count : int
  ; http_status : int option
  ; provider_trace : provider_trace option
  ; call_id : call_id
  ; plan_fingerprint : string
  ; request_body_sha256 : string
  ; catalog_generation : Exact_output_resolver.catalog_generation
  ; catalog_evidence : Exact_output_resolver.catalog_evidence
  ; target_identity : Exact_output_resolver.target_identity
  }

let create
      ~call_id
      ~plan_fingerprint
      ~request_body_sha256
      ~catalog_generation
      ~catalog_evidence
      ~target_identity
  =
  { state = Atomic.make Not_started_state
  ; call_id
  ; plan_fingerprint
  ; request_body_sha256
  ; catalog_generation
  ; catalog_evidence
  ; target_identity
  }
;;

let try_start receipt =
  Atomic.compare_and_set receipt.state Not_started_state Before_dispatch_state
;;

let call_id (receipt : t) = receipt.call_id

let phase receipt =
  match Atomic.get receipt.state with
  | Not_started_state -> Not_started
  | Before_dispatch_state -> Before_dispatch
  | Dispatch_started_state -> Dispatch_started
  | Response_received_state _ -> Response_received
  | Terminal_state _ -> Terminal
;;

let dispatch_count receipt =
  match Atomic.get receipt.state with
  | Not_started_state | Before_dispatch_state -> 0
  | Dispatch_started_state | Response_received_state _ | Terminal_state _ -> 1
;;

let generation_dispatched receipt = dispatch_count receipt = 1

let http_status receipt =
  match Atomic.get receipt.state with
  | Response_received_state received -> received.status
  | Terminal_state terminal -> Some terminal.status
  | Not_started_state | Before_dispatch_state | Dispatch_started_state -> None
;;

let provider_trace receipt =
  match Atomic.get receipt.state with
  | Terminal_state terminal -> terminal.provider_trace
  | Response_received_state received -> received.provider_trace
  | Not_started_state | Before_dispatch_state | Dispatch_started_state -> None
;;

let plan_fingerprint (receipt : t) = receipt.plan_fingerprint
let request_body_sha256 (receipt : t) = receipt.request_body_sha256
let catalog_generation (receipt : t) = receipt.catalog_generation
let catalog_evidence (receipt : t) = receipt.catalog_evidence
let target_identity (receipt : t) = receipt.target_identity

let state_rank = function
  | Not_started_state -> 0
  | Before_dispatch_state -> 1
  | Dispatch_started_state -> 2
  | Response_received_state _ -> 3
  | Terminal_state _ -> 4
;;

(* Advancing must never drop what the receipt already knows. Within one rank the
   desired value is merged into the current one rather than replacing it: a status
   observed after a trace was recorded, or a trace recorded before the status
   arrived, both have to survive. Replacing outright is how the earlier
   [Response_received_state None -> Some _] special case worked, and it silently
   lost the trace once this state gained a second field. *)
let rec advance receipt desired =
  let current = Atomic.get receipt.state in
  if state_rank desired > state_rank current
  then
    if not (Atomic.compare_and_set receipt.state current desired)
    then advance receipt desired
  else if state_rank desired = state_rank current
  then (
    (* The one same-rank gain, kept from the original [Response_received_state None
       -> Some _] rule but written as a merge. Replacing the whole value would drop a
       provider trace recorded before the status arrived, which became possible when
       this state gained a second field. Comparison stays structural on the status
       option only: the trace has its own [Trace.equal] because polymorphic compare
       is not safe on it. *)
    match current, desired with
    | Response_received_state received, Response_received_state incoming
      when Option.is_none received.status && Option.is_some incoming.status ->
      let merged = Response_received_state { received with status = incoming.status } in
      if not (Atomic.compare_and_set receipt.state current merged)
      then advance receipt desired
    | _ -> ())
;;

let observe_phase receipt = function
  | Http_client_phase_observer.Dispatch_started -> advance receipt Dispatch_started_state
  | Http_client_phase_observer.Response_received status ->
    advance receipt (Response_received_state { status = Some status; provider_trace = None })
;;

let synchronize receipt complete_receipt =
  match Exec.receipt_phase complete_receipt with
  | Exec.Before_dispatch -> advance receipt Before_dispatch_state
  | Exec.Dispatch_started -> advance receipt Dispatch_started_state
  | Exec.Response_received ->
    advance
      receipt
      (Response_received_state
         { status = Exec.receipt_http_status complete_receipt; provider_trace = None })
  | Exec.Terminal ->
    (match Exec.receipt_http_status complete_receipt with
     | Some status -> advance receipt (Terminal_state { status; provider_trace = None })
     | None -> invalid_arg "Exact_output: terminal receipt without HTTP status")
;;

let rec record_provider_trace receipt provider_trace =
  let current = Atomic.get receipt.state in
  match current with
  | Terminal_state ({ provider_trace = None; _ } as terminal) ->
    let desired = Terminal_state { terminal with provider_trace = Some provider_trace } in
    if not (Atomic.compare_and_set receipt.state current desired)
    then record_provider_trace receipt provider_trace
  | Terminal_state { provider_trace = Some recorded; _ } ->
    if not (Trace.equal recorded provider_trace)
    then invalid_arg "Exact_output: conflicting provider trace"
  | Response_received_state ({ provider_trace = None; _ } as received) ->
    let desired =
      Response_received_state { received with provider_trace = Some provider_trace }
    in
    if not (Atomic.compare_and_set receipt.state current desired)
    then record_provider_trace receipt provider_trace
  | Response_received_state { provider_trace = Some recorded; _ } ->
    if not (Trace.equal recorded provider_trace)
    then invalid_arg "Exact_output: conflicting provider trace"
  | Not_started_state | Before_dispatch_state | Dispatch_started_state ->
    invalid_arg "Exact_output: provider trace before response is received"
;;

let snapshot receipt =
  let state = Atomic.get receipt.state in
  let phase, dispatch_count, http_status, provider_trace =
    match state with
    | Not_started_state -> Not_started, 0, None, None
    | Before_dispatch_state -> Before_dispatch, 0, None, None
    | Dispatch_started_state -> Dispatch_started, 1, None, None
    | Response_received_state received ->
      Response_received, 1, received.status, received.provider_trace
    | Terminal_state terminal ->
      Terminal, 1, Some terminal.status, terminal.provider_trace
  in
  { phase
  ; dispatch_count
  ; http_status
  ; provider_trace
  ; call_id = receipt.call_id
  ; plan_fingerprint = receipt.plan_fingerprint
  ; request_body_sha256 = receipt.request_body_sha256
  ; catalog_generation = receipt.catalog_generation
  ; catalog_evidence = receipt.catalog_evidence
  ; target_identity = receipt.target_identity
  }
;;

let snapshot_phase (snapshot : snapshot) = snapshot.phase
let snapshot_dispatch_count (snapshot : snapshot) = snapshot.dispatch_count
let snapshot_http_status (snapshot : snapshot) = snapshot.http_status
let snapshot_provider_trace (snapshot : snapshot) = snapshot.provider_trace
let snapshot_call_id (snapshot : snapshot) = snapshot.call_id
let snapshot_plan_fingerprint (snapshot : snapshot) = snapshot.plan_fingerprint
let snapshot_request_body_sha256 (snapshot : snapshot) = snapshot.request_body_sha256
let snapshot_catalog_generation (snapshot : snapshot) = snapshot.catalog_generation
let snapshot_catalog_evidence (snapshot : snapshot) = snapshot.catalog_evidence
let snapshot_target_identity (snapshot : snapshot) = snapshot.target_identity
