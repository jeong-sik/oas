(** Working memory: 3-tier facade over {!Context.t}.

    Formalizes the scratchpad/working/long-term pattern on top of
    the existing scoped key-value store. Each tier maps to a
    {!Context.scope}:

    - {b Scratchpad} ([Temp]) — per-turn ephemeral data.
    - {b Working} ([Session]) — promoted from scratchpad, survives turns.
    - {b Long_term} — external persistence via callback.

    Design: thin facade. All data lives in {!Context.t}; this module
    adds tier semantics and a promotion API. No new storage backend.

    @since 0.65.0 *)

type tier =
  | Scratchpad   (** Per-turn, cleared between turns *)
  | Working      (** Cross-turn, survives within session *)
  | Long_term    (** External persistence via callback *)

(** Callback for long-term memory operations. *)
type long_term_backend = {
  persist: key:string -> Yojson.Safe.t -> unit;
  retrieve: key:string -> Yojson.Safe.t option;
  remove: key:string -> unit;
}

type t = {
  ctx: Context.t;
  mutable long_term: long_term_backend option;
}

let scope_of_tier = function
  | Scratchpad -> Context.Temp
  | Working -> Context.Session
  | Long_term -> Context.Custom "lt"

let create ?(ctx = Context.create ()) ?long_term () =
  { ctx; long_term }

let set_long_term_backend t backend =
  t.long_term <- Some backend

(** Store a value at the given tier. *)
let store t ~tier key value =
  match tier with
  | Long_term ->
    Context.set_scoped t.ctx (scope_of_tier Long_term) key value;
    (match t.long_term with
     | Some backend -> backend.persist ~key value
     | None -> ())
  | _ ->
    Context.set_scoped t.ctx (scope_of_tier tier) key value

(** Recall a value. Checks the given tier first, then falls back
    through lower tiers: Scratchpad -> Working -> Long_term. *)
let recall t ~tier key =
  match Context.get_scoped t.ctx (scope_of_tier tier) key with
  | Some _ as found -> found
  | None ->
    match tier with
    | Scratchpad ->
      (* Fall back to Working, then Long_term *)
      (match Context.get_scoped t.ctx (scope_of_tier Working) key with
       | Some _ as found -> found
       | None ->
         match t.long_term with
         | Some backend -> backend.retrieve ~key
         | None -> Context.get_scoped t.ctx (scope_of_tier Long_term) key)
    | Working ->
      (* Fall back to Long_term *)
      (match t.long_term with
       | Some backend -> backend.retrieve ~key
       | None -> Context.get_scoped t.ctx (scope_of_tier Long_term) key)
    | Long_term ->
      (* Check backend *)
      (match t.long_term with
       | Some backend -> backend.retrieve ~key
       | None -> None)

(** Recall from a specific tier only, no fallback. *)
let recall_exact t ~tier key =
  match tier with
  | Long_term ->
    (match t.long_term with
     | Some backend -> backend.retrieve ~key
     | None -> Context.get_scoped t.ctx (scope_of_tier Long_term) key)
  | _ ->
    Context.get_scoped t.ctx (scope_of_tier tier) key

(** Remove a key from the given tier. *)
let forget t ~tier key =
  match tier with
  | Long_term ->
    Context.delete_scoped t.ctx (scope_of_tier Long_term) key;
    (match t.long_term with
     | Some backend -> backend.remove ~key
     | None -> ())
  | _ ->
    Context.delete_scoped t.ctx (scope_of_tier tier) key

(** Promote a key from Scratchpad to Working. *)
let promote t key =
  match Context.get_scoped t.ctx (scope_of_tier Scratchpad) key with
  | Some value ->
    Context.set_scoped t.ctx (scope_of_tier Working) key value;
    Context.delete_scoped t.ctx (scope_of_tier Scratchpad) key;
    true
  | None -> false

(** Get all entries in the Working tier. *)
let working_entries t =
  Context.keys_in_scope t.ctx (scope_of_tier Working)
  |> List.filter_map (fun key ->
    match Context.get_scoped t.ctx (scope_of_tier Working) key with
    | Some v -> Some (key, v)
    | None -> None)

(** Get all entries in the Scratchpad tier. *)
let scratchpad_entries t =
  Context.keys_in_scope t.ctx (scope_of_tier Scratchpad)
  |> List.filter_map (fun key ->
    match Context.get_scoped t.ctx (scope_of_tier Scratchpad) key with
    | Some v -> Some (key, v)
    | None -> None)

(** Clear all Scratchpad entries (call between turns). *)
let clear_scratchpad t =
  let keys = Context.keys_in_scope t.ctx (scope_of_tier Scratchpad) in
  List.iter (fun key ->
    Context.delete_scoped t.ctx (scope_of_tier Scratchpad) key
  ) keys

(** Count entries per tier. *)
let stats t =
  let scratchpad = List.length (Context.keys_in_scope t.ctx (scope_of_tier Scratchpad)) in
  let working = List.length (Context.keys_in_scope t.ctx (scope_of_tier Working)) in
  let long_term = List.length (Context.keys_in_scope t.ctx (scope_of_tier Long_term)) in
  (scratchpad, working, long_term)

(** Access the underlying context. *)
let context t = t.ctx
