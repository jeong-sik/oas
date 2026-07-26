type state =
  | Not_started
  | Running
  | Terminal

type t = state Atomic.t

type ('admission, 'attempt, 'measurement) progress_snapshot =
  { candidate_visit_count : int
  ; admissions : 'admission list
  ; attempts : 'attempt list
  ; measurements : 'measurement list
  }

type ('admission, 'attempt, 'measurement) progress_state =
  { candidate_visit_count : int
  ; admissions_rev : 'admission list
  ; attempts_rev : 'attempt list
  ; measurements_rev : 'measurement list
  }

type ('admission, 'attempt, 'measurement) progress =
  ('admission, 'attempt, 'measurement) progress_state Atomic.t

type preference_reservation = Preference_reservation of int64
type success_ordinal = Success_ordinal of int64

type 'candidate preference_entry =
  { mutable reservation : preference_reservation
  ; mutable preference : ('candidate * success_ordinal) option
  }

type preference_lifecycle =
  | Recovering
  | Active

type domain_disposition =
  | Valid
  | Rejected

type domain_settlement_receipt =
  { settlement_id : string
  ; disposition : domain_disposition
  }

type ('scope, 'candidate) preference_store =
  { mutex : Mutex.t
  ; capacity : int
  ; entries : ('scope, 'candidate preference_entry) Hashtbl.t
  ; recovered_domains : (string, domain_disposition) Hashtbl.t
  ; mutable lifecycle : preference_lifecycle
  ; mutable last_reservation_ordinal : int64
  ; mutable last_success_ordinal : int64
  }

type ('scope, 'candidate) preference_recovery = ('scope, 'candidate) preference_store

type settlement_state =
  | Pending
  | Publishing of domain_settlement_receipt
  | Settled of domain_settlement_receipt

type domain_settlement = settlement_state Atomic.t
type preference_store_error = Invalid_preference_capacity of int

type preference_reservation_error =
  | Preference_capacity_exhausted of { capacity : int }
  | Preference_reservation_exhausted

type preference_recovery_error = Preference_recovery_already_finished

type preference_scope_removal =
  | Preference_scope_removed
  | Preference_scope_not_reserved

type success_ordinal_error = Success_ordinal_exhausted

type domain_settlement_begin =
  | Domain_settlement_claimed
  | Domain_settlement_replayed of domain_settlement_receipt
  | Domain_settlement_in_progress
  | Domain_settlement_conflict

type domain_settlement_error = Domain_settlement_apply_conflict

type recovery_domain_error =
  | Preference_recovery_finished
  | Preference_recovery_capacity_exhausted of { capacity : int }
  | Recovered_domain_conflict

type ('candidate, 'success, 'execution_error, 'advanceable_error, 'callback_error) outcome =
  | Succeeded of
      { candidate : 'candidate
      ; success : 'success
      }
  | Attempt_already_started
  | Before_advance_callback_failed of
      { failed_candidate : 'candidate
      ; failure : 'advanceable_error
      ; next_candidate : 'candidate
      ; cause : 'callback_error
      }
  | Execution_failed of
      { candidate : 'candidate
      ; cause : 'execution_error
      }

let create () = Atomic.make Not_started

let create_progress () =
  Atomic.make
    { candidate_visit_count = 0
    ; admissions_rev = []
    ; attempts_rev = []
    ; measurements_rev = []
    }
;;

let start_preference_recovery ~capacity =
  if capacity <= 0
  then Error (Invalid_preference_capacity capacity)
  else
    Ok
      { mutex = Mutex.create ()
      ; capacity
      ; entries = Hashtbl.create (Int.min capacity 16)
      ; recovered_domains = Hashtbl.create 16
      ; lifecycle = Recovering
      ; last_reservation_ordinal = 0L
      ; last_success_ordinal = 0L
      }
;;

let create_domain_settlement () = Atomic.make Pending

let with_preference_lock (store : (_, _) preference_store) f =
  Mutex.lock store.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock store.mutex) f
;;

let finish_preference_recovery recovery =
  with_preference_lock recovery (fun () ->
    match recovery.lifecycle with
    | Active -> Error Preference_recovery_already_finished
    | Recovering ->
      recovery.lifecycle <- Active;
      Hashtbl.clear recovery.recovered_domains;
      Ok recovery)
;;

let reserve_preference_scope store ~scope =
  with_preference_lock store (fun () ->
    match Hashtbl.find_opt store.entries scope with
    | Some entry -> Ok (entry.reservation, entry.preference)
    | None ->
      if Hashtbl.length store.entries >= store.capacity
      then Error (Preference_capacity_exhausted { capacity = store.capacity })
      else if Int64.equal store.last_reservation_ordinal Int64.max_int
      then Error Preference_reservation_exhausted
      else (
        let ordinal = Int64.succ store.last_reservation_ordinal in
        store.last_reservation_ordinal <- ordinal;
        let reservation = Preference_reservation ordinal in
        Hashtbl.add store.entries scope { reservation; preference = None };
        Ok (reservation, None)))
;;

let remove_preference_scope store ~scope =
  with_preference_lock store (fun () ->
    if Hashtbl.mem store.entries scope
    then (
      Hashtbl.remove store.entries scope;
      Preference_scope_removed)
    else Preference_scope_not_reserved)
;;

let allocate_success_ordinal store =
  with_preference_lock store (fun () ->
    if Int64.equal store.last_success_ordinal Int64.max_int
    then Error Success_ordinal_exhausted
    else (
      let ordinal = Int64.succ store.last_success_ordinal in
      store.last_success_ordinal <- ordinal;
      Ok (Success_ordinal ordinal)))
;;

let preference_reservation_to_int64 (Preference_reservation ordinal) = ordinal

let preference_reservation_of_int64 ordinal =
  if Int64.compare ordinal 0L <= 0 then None else Some (Preference_reservation ordinal)
;;

let success_ordinal_to_int64 (Success_ordinal ordinal) = ordinal

let success_ordinal_of_int64 ordinal =
  if Int64.compare ordinal 0L <= 0 then None else Some (Success_ordinal ordinal)
;;

let compare_success_ordinal (Success_ordinal left) (Success_ordinal right) =
  Int64.compare left right
;;

let record_preference_locked store ~scope ~reservation ~candidate ~ordinal =
  match Hashtbl.find_opt store.entries scope with
  | None -> ()
  | Some entry when entry.reservation <> reservation -> ()
  | Some { preference = Some (current_candidate, current_ordinal); _ }
    when compare_success_ordinal ordinal current_ordinal <= 0 -> ignore current_candidate
  | Some entry -> entry.preference <- Some (candidate, ordinal)
;;

let same_disposition left right =
  match left, right with
  | Valid, Valid | Rejected, Rejected -> true
  | Valid, Rejected | Rejected, Valid -> false
;;

let same_receipt left right =
  left.settlement_id = right.settlement_id
  && same_disposition left.disposition right.disposition
;;

let begin_domain_settlement settlement preferences requested =
  with_preference_lock preferences (fun () ->
    match Atomic.get settlement with
    | Pending ->
      Atomic.set settlement (Publishing requested);
      Domain_settlement_claimed
    | Settled receipt ->
      if same_receipt receipt requested
      then Domain_settlement_replayed receipt
      else Domain_settlement_conflict
    | Publishing receipt ->
      if same_receipt receipt requested
      then Domain_settlement_in_progress
      else Domain_settlement_conflict)
;;

let abort_domain_settlement settlement preferences requested =
  with_preference_lock preferences (fun () ->
    match Atomic.get settlement with
    | Publishing receipt when same_receipt receipt requested ->
      Atomic.set settlement Pending
    | Pending | Publishing _ | Settled _ -> ())
;;

let finish_domain_settlement
      settlement
      preferences
      ~scope
      ~reservation
      ~candidate
      ~ordinal
      requested
  =
  with_preference_lock preferences (fun () ->
    match Atomic.get settlement with
    | Publishing receipt when same_receipt receipt requested ->
      (match requested.disposition with
       | Rejected -> ()
       | Valid ->
         record_preference_locked preferences ~scope ~reservation ~candidate ~ordinal);
      Atomic.set settlement (Settled requested);
      Ok requested
    | Settled receipt when same_receipt receipt requested -> Ok receipt
    | Pending | Publishing _ | Settled _ -> Error Domain_settlement_apply_conflict)
;;

let max_int64 left right = if Int64.compare left right >= 0 then left else right

let install_recovered_preference_locked recovery ~scope ~reservation ~candidate ~ordinal =
  match Hashtbl.find_opt recovery.entries scope with
  | None ->
    Hashtbl.add
      recovery.entries
      scope
      { reservation; preference = Some (candidate, ordinal) }
  | Some entry ->
    let current = preference_reservation_to_int64 entry.reservation in
    let incoming = preference_reservation_to_int64 reservation in
    if Int64.compare incoming current > 0
    then (
      entry.reservation <- reservation;
      entry.preference <- Some (candidate, ordinal))
    else if Int64.equal incoming current
    then (
      match entry.preference with
      | Some (_, current_ordinal)
        when compare_success_ordinal ordinal current_ordinal <= 0 -> ()
      | None | Some _ -> entry.preference <- Some (candidate, ordinal))
;;

let resume_committed_domain recovery ~scope ~reservation ~candidate ~ordinal receipt =
  with_preference_lock recovery (fun () ->
    match recovery.lifecycle with
    | Active -> Error Preference_recovery_finished
    | Recovering ->
      (match Hashtbl.find_opt recovery.recovered_domains receipt.settlement_id with
       | Some disposition when same_disposition disposition receipt.disposition ->
         Ok receipt
       | Some _ -> Error Recovered_domain_conflict
       | None
         when receipt.disposition = Valid
              && (not (Hashtbl.mem recovery.entries scope))
              && Hashtbl.length recovery.entries >= recovery.capacity ->
         Error (Preference_recovery_capacity_exhausted { capacity = recovery.capacity })
       | None ->
         recovery.last_reservation_ordinal
         <- max_int64
              recovery.last_reservation_ordinal
              (preference_reservation_to_int64 reservation);
         recovery.last_success_ordinal
         <- max_int64 recovery.last_success_ordinal (success_ordinal_to_int64 ordinal);
         (match receipt.disposition with
          | Rejected -> ()
          | Valid ->
            install_recovered_preference_locked
              recovery
              ~scope
              ~reservation
              ~candidate
              ~ordinal);
         Hashtbl.add recovery.recovered_domains receipt.settlement_id receipt.disposition;
         Ok receipt))
;;

let record_admission progress admission =
  let current = Atomic.get progress in
  let candidate_visit_count = current.candidate_visit_count + 1 in
  Atomic.set
    progress
    { current with
      candidate_visit_count
    ; admissions_rev = admission :: current.admissions_rev
    }
;;

let record_attempt progress attempt =
  let current = Atomic.get progress in
  Atomic.set progress { current with attempts_rev = attempt :: current.attempts_rev }
;;

let rec replace_or_prepend ~same value = function
  | [] -> [ value ]
  | head :: tail when same head value -> value :: tail
  | head :: tail -> head :: replace_or_prepend ~same value tail
;;

let publish_attempt progress ~same attempt =
  let current = Atomic.get progress in
  Atomic.set
    progress
    { current with attempts_rev = replace_or_prepend ~same attempt current.attempts_rev }
;;

let publish_measurement progress ~same measurement =
  let current = Atomic.get progress in
  Atomic.set
    progress
    { current with
      measurements_rev = replace_or_prepend ~same measurement current.measurements_rev
    }
;;

let progress_snapshot progress =
  let current = Atomic.get progress in
  { candidate_visit_count = current.candidate_visit_count
  ; admissions = List.rev current.admissions_rev
  ; attempts = List.rev current.attempts_rev
  ; measurements = List.rev current.measurements_rev
  }
;;

let duplicate_key ~equal ~key candidates =
  let rec find position seen = function
    | [] -> None
    | candidate :: rest ->
      let value = key candidate in
      (match List.find_opt (fun (seen_value, _) -> equal seen_value value) seen with
       | Some (_, first_position) -> Some (value, first_position, position)
       | None -> find (position + 1) ((value, position) :: seen) rest)
  in
  find 1 [] candidates
;;

let promote_candidate ~equal ~key ~preferred candidates =
  match preferred with
  | None -> candidates
  | Some preferred ->
    let selected, remaining =
      List.partition (fun candidate -> equal (key candidate) preferred) candidates
    in
    selected @ remaining
;;

let execute_once state ~candidates ~execute ~advanceable ~before_advance =
  if not (Atomic.compare_and_set state Not_started Running)
  then Attempt_already_started
  else
    Fun.protect
      ~finally:(fun () -> Atomic.set state Terminal)
      (fun () ->
         let rec execute_candidates = function
           | [] -> invalid_arg "Exact_output_flow: empty candidate snapshot"
           | candidate :: rest ->
             (match execute candidate with
              | Ok success -> Succeeded { candidate; success }
              | Error failure ->
                (match rest, advanceable failure with
                 | next :: _, Some advanceable_failure ->
                   (match
                      before_advance ~failed:candidate ~failure:advanceable_failure ~next
                    with
                    | Ok () -> execute_candidates rest
                    | Error cause ->
                      Before_advance_callback_failed
                        { failed_candidate = candidate
                        ; failure = advanceable_failure
                        ; next_candidate = next
                        ; cause
                        })
                 | [], _ | _ :: _, None -> Execution_failed { candidate; cause = failure }))
         in
         execute_candidates candidates)
;;
