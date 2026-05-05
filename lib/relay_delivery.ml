(** Two-stage event relay delivery. *)

type stage =
  | Persist
  | Publish
  | Queue

type 'a pending =
  { payload : 'a
  ; attempts : int
  ; persisted : bool
  }

type 'a delivery_result =
  | Delivered
  | Retryable_failure of 'a pending * stage * exn

type stats =
  { queue_depth : int
  ; retry_total : int
  ; drop_total : int
  ; retry_persist_total : int
  ; retry_publish_total : int
  ; drop_persist_total : int
  ; drop_publish_total : int
  ; drop_queue_total : int
  }

type 'a t =
  { max_attempts : int
  ; max_queue_depth : int
  ; mutable pending : 'a pending list
  ; mutable retry_persist_total : int
  ; mutable retry_publish_total : int
  ; mutable drop_persist_total : int
  ; mutable drop_publish_total : int
  ; mutable drop_queue_total : int
  }

let stage_to_string = function
  | Persist -> "persist"
  | Publish -> "publish"
  | Queue -> "queue"
;;

let make_pending payload = { payload; attempts = 0; persisted = false }

let deliver_with ~persist ~publish pending =
  try
    let pending =
      if pending.persisted
      then pending
      else (
        persist pending.payload;
        { pending with persisted = true })
    in
    try
      publish pending.payload;
      Delivered
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn -> Retryable_failure (pending, Publish, exn)
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Retryable_failure (pending, Persist, exn)
;;

let create ?(max_attempts = 3) ?(max_queue_depth = 256) () =
  { max_attempts = max 1 max_attempts
  ; max_queue_depth = max 1 max_queue_depth
  ; pending = []
  ; retry_persist_total = 0
  ; retry_publish_total = 0
  ; drop_persist_total = 0
  ; drop_publish_total = 0
  ; drop_queue_total = 0
  }
;;

let enqueue t payload =
  let item = make_pending payload in
  if List.length t.pending < t.max_queue_depth
  then (
    t.pending <- t.pending @ [ item ];
    None)
  else (
    match t.pending with
    | dropped :: rest ->
      t.pending <- rest @ [ item ];
      t.drop_queue_total <- t.drop_queue_total + 1;
      Some dropped
    | [] ->
      t.pending <- [ item ];
      None)
;;

let incr_retry t = function
  | Persist -> t.retry_persist_total <- t.retry_persist_total + 1
  | Publish -> t.retry_publish_total <- t.retry_publish_total + 1
  | Queue -> ()
;;

let incr_drop t = function
  | Persist -> t.drop_persist_total <- t.drop_persist_total + 1
  | Publish -> t.drop_publish_total <- t.drop_publish_total + 1
  | Queue -> t.drop_queue_total <- t.drop_queue_total + 1
;;

let process_once t ~persist ~publish =
  let rec loop acc = function
    | [] -> List.rev acc
    | pending :: rest ->
      (match deliver_with ~persist ~publish pending with
       | Delivered -> loop acc rest
       | Retryable_failure (pending, stage, _exn) ->
         let attempts = pending.attempts + 1 in
         if attempts >= t.max_attempts
         then (
           incr_drop t stage;
           loop acc rest)
         else (
           incr_retry t stage;
           loop ({ pending with attempts } :: acc) rest))
  in
  t.pending <- loop [] t.pending
;;

let pending t = t.pending

let stats t =
  let retry_total = t.retry_persist_total + t.retry_publish_total in
  let drop_total =
    t.drop_persist_total + t.drop_publish_total + t.drop_queue_total
  in
  { queue_depth = List.length t.pending
  ; retry_total
  ; drop_total
  ; retry_persist_total = t.retry_persist_total
  ; retry_publish_total = t.retry_publish_total
  ; drop_persist_total = t.drop_persist_total
  ; drop_publish_total = t.drop_publish_total
  ; drop_queue_total = t.drop_queue_total
  }
;;

let health_probe ?checked_at ?(name = "event_relay") stats =
  let status =
    if stats.drop_total > 0
    then Runtime_health.Degraded
    else if stats.queue_depth > 0 || stats.retry_total > 0
    then Runtime_health.Degraded
    else Runtime_health.Status_ok
  in
  let detail =
    Printf.sprintf
      "queue_depth=%d retry_total=%d drop_total=%d"
      stats.queue_depth
      stats.retry_total
      stats.drop_total
  in
  Runtime_health.make_probe
    ~name:(Runtime_health.Custom name)
    ~status
    ~detail
    ?checked_at
    ()
;;
