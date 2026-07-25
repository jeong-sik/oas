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

type preference_reservation = unit ref
type success_ordinal = Success_ordinal of int64

type 'candidate preference_entry =
  { reservation : preference_reservation
  ; mutable preference : ('candidate * success_ordinal) option
  }

type ('scope, 'candidate) preference_store =
  { mutex : Mutex.t
  ; capacity : int
  ; entries : ('scope, 'candidate preference_entry) Hashtbl.t
  ; mutable last_success_ordinal : int64
  }

type settlement_state =
  | Pending
  | Publishing
  | Settled

type domain_settlement = settlement_state Atomic.t
type preference_store_error = Invalid_preference_capacity of int
type preference_reservation_error = Preference_capacity_exhausted of { capacity : int }

type preference_scope_removal =
  | Preference_scope_removed
  | Preference_scope_not_reserved

type success_ordinal_error = Success_ordinal_exhausted

type domain_settlement_error =
  | Already_settled
  | Preference_scope_released

type preference_record_error = Preference_scope_not_reserved_for_record

type 'candidate preference_installation =
  | Preference_installed
  | Preference_superseded of
      { current_candidate : 'candidate
      ; current_ordinal : success_ordinal
      }

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

let create_preference_store ~capacity =
  if capacity <= 0
  then Error (Invalid_preference_capacity capacity)
  else
    Ok
      { mutex = Mutex.create ()
      ; capacity
      ; entries = Hashtbl.create (Int.min capacity 16)
      ; last_success_ordinal = 0L
      }
;;

let create_domain_settlement () = Atomic.make Pending

let with_preference_lock (store : (_, _) preference_store) f =
  Mutex.lock store.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock store.mutex) f
;;

let reserve_preference_scope store ~scope =
  with_preference_lock store (fun () ->
    match Hashtbl.find_opt store.entries scope with
    | Some entry -> Ok (entry.reservation, entry.preference)
    | None ->
      if Hashtbl.length store.entries >= store.capacity
      then Error (Preference_capacity_exhausted { capacity = store.capacity })
      else (
        let reservation = ref () in
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

let success_ordinal_to_int64 (Success_ordinal ordinal) = ordinal

let compare_success_ordinal (Success_ordinal left) (Success_ordinal right) =
  Int64.compare left right
;;

let record_preference_locked store ~scope ~reservation ~candidate ~ordinal =
  match Hashtbl.find_opt store.entries scope with
  | None -> Error Preference_scope_not_reserved_for_record
  | Some entry when entry.reservation != reservation ->
    Error Preference_scope_not_reserved_for_record
  | Some { preference = Some (current_candidate, current_ordinal); _ }
    when compare_success_ordinal ordinal current_ordinal <= 0 ->
    Ok (Preference_superseded { current_candidate; current_ordinal })
  | Some entry ->
    entry.preference <- Some (candidate, ordinal);
    Ok Preference_installed
;;

let settle_domain_rejected_once_with_publication_hook
      ~after_failed_cas
      settlement
      preferences
  =
  if Atomic.compare_and_set settlement Pending Settled
  then Ok ()
  else (
    after_failed_cas ();
    with_preference_lock preferences (fun () -> ());
    Error Already_settled)
;;

let settle_domain_rejected_once settlement preferences =
  settle_domain_rejected_once_with_publication_hook
    ~after_failed_cas:ignore
    settlement
    preferences
;;

let settle_domain_valid_once_with_publication_hook
      ~after_publishing
      settlement
      preferences
      ~scope
      ~reservation
      ~candidate
      ~ordinal
  =
  with_preference_lock preferences (fun () ->
    if not (Atomic.compare_and_set settlement Pending Publishing)
    then Error Already_settled
    else
      Fun.protect
        ~finally:(fun () -> Atomic.set settlement Settled)
        (fun () ->
           after_publishing ();
           match
             record_preference_locked preferences ~scope ~reservation ~candidate ~ordinal
           with
           | Ok installation -> Ok installation
           | Error Preference_scope_not_reserved_for_record ->
             Error Preference_scope_released))
;;

let settle_domain_valid_once
      settlement
      preferences
      ~scope
      ~reservation
      ~candidate
      ~ordinal
  =
  settle_domain_valid_once_with_publication_hook
    ~after_publishing:ignore
    settlement
    preferences
    ~scope
    ~reservation
    ~candidate
    ~ordinal
;;

let%test "domain-valid publication blocks a rejected loser and its immediate snapshot" =
  match create_preference_store ~capacity:1 with
  | Error _ -> false
  | Ok preferences ->
    let scope = "valid-wins" in
    (match reserve_preference_scope preferences ~scope with
     | Error _ | Ok (_, Some _) -> false
     | Ok (reservation, None) ->
       (match allocate_success_ordinal preferences with
        | Error _ -> false
        | Ok ordinal ->
          let settlement = create_domain_settlement () in
          let publishing = Atomic.make false in
          let release_publication = Atomic.make false in
          let rejected_cas_lost = Atomic.make false in
          let loser_finished = Atomic.make false in
          let winner =
            Domain.spawn (fun () ->
              settle_domain_valid_once_with_publication_hook
                ~after_publishing:(fun () ->
                  Atomic.set publishing true;
                  while not (Atomic.get release_publication) do
                    Domain.cpu_relax ()
                  done)
                settlement
                preferences
                ~scope
                ~reservation
                ~candidate:"winner"
                ~ordinal)
          in
          while not (Atomic.get publishing) do
            Domain.cpu_relax ()
          done;
          let loser =
            Domain.spawn (fun () ->
              let result =
                settle_domain_rejected_once_with_publication_hook
                  ~after_failed_cas:(fun () -> Atomic.set rejected_cas_lost true)
                  settlement
                  preferences
              in
              let snapshot = reserve_preference_scope preferences ~scope in
              Atomic.set loser_finished true;
              result, snapshot)
          in
          while not (Atomic.get rejected_cas_lost) do
            Domain.cpu_relax ()
          done;
          let returned_before_publication = Atomic.get loser_finished in
          Atomic.set release_publication true;
          let winner_result = Domain.join winner in
          let loser_result, snapshot = Domain.join loser in
          (not returned_before_publication)
          &&
            (match winner_result, loser_result, snapshot with
            | ( Ok Preference_installed
              , Error Already_settled
              , Ok (_, Some (candidate, installed_ordinal)) ) ->
              candidate = "winner"
              && Int64.equal
                   (success_ordinal_to_int64 ordinal)
                   (success_ordinal_to_int64 installed_ordinal)
            | _ -> false)))
;;

let%test "domain rejection can deterministically win against domain valid" =
  match create_preference_store ~capacity:1 with
  | Error _ -> false
  | Ok preferences ->
    let scope = "rejected-wins" in
    (match reserve_preference_scope preferences ~scope with
     | Error _ | Ok (_, Some _) -> false
     | Ok (reservation, None) ->
       (match allocate_success_ordinal preferences with
        | Error _ -> false
        | Ok ordinal ->
          let settlement = create_domain_settlement () in
          let start = Atomic.make false in
          let rejection_committed = Atomic.make false in
          let rejected =
            Domain.spawn (fun () ->
              while not (Atomic.get start) do
                Domain.cpu_relax ()
              done;
              let result = settle_domain_rejected_once settlement preferences in
              Atomic.set rejection_committed true;
              result)
          in
          let valid =
            Domain.spawn (fun () ->
              while not (Atomic.get start) do
                Domain.cpu_relax ()
              done;
              while not (Atomic.get rejection_committed) do
                Domain.cpu_relax ()
              done;
              settle_domain_valid_once
                settlement
                preferences
                ~scope
                ~reservation
                ~candidate:"loser"
                ~ordinal)
          in
          Atomic.set start true;
          let rejected_result = Domain.join rejected in
          let valid_result = Domain.join valid in
          let snapshot = reserve_preference_scope preferences ~scope in
          (match rejected_result, valid_result, snapshot with
           | Ok (), Error Already_settled, Ok (_, None) -> true
           | _ -> false)))
;;

let%test "two concurrent domain rejections have exactly one winner" =
  match create_preference_store ~capacity:1 with
  | Error _ -> false
  | Ok preferences ->
    let settlement = create_domain_settlement () in
    let ready = Atomic.make 0 in
    let start = Atomic.make false in
    let reject () =
      ignore (Atomic.fetch_and_add ready 1);
      while not (Atomic.get start) do
        Domain.cpu_relax ()
      done;
      settle_domain_rejected_once settlement preferences
    in
    let left = Domain.spawn reject in
    let right = Domain.spawn reject in
    while Atomic.get ready <> 2 do
      Domain.cpu_relax ()
    done;
    Atomic.set start true;
    (match Domain.join left, Domain.join right with
     | Ok (), Error Already_settled | Error Already_settled, Ok () -> true
     | _ -> false)
;;

let%test "exception after Publishing terminalizes settlement before store unlock" =
  let exception Injected_after_publishing in
  match create_preference_store ~capacity:1 with
  | Error _ -> false
  | Ok preferences ->
    let scope = "publishing-exception" in
    (match reserve_preference_scope preferences ~scope with
     | Error _ | Ok (_, Some _) -> false
     | Ok (reservation, None) ->
       (match allocate_success_ordinal preferences with
        | Error _ -> false
        | Ok ordinal ->
          let settlement = create_domain_settlement () in
          let raised =
            try
              ignore
                (settle_domain_valid_once_with_publication_hook
                   ~after_publishing:(fun () -> raise Injected_after_publishing)
                   settlement
                   preferences
                   ~scope
                   ~reservation
                   ~candidate:"never-installed"
                   ~ordinal);
              false
            with
            | Injected_after_publishing -> true
          in
          let duplicate = settle_domain_rejected_once settlement preferences in
          let snapshot = reserve_preference_scope preferences ~scope in
          raised
          &&
            (match duplicate, snapshot with
            | Error Already_settled, Ok (_, None) -> true
            | _ -> false)))
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
