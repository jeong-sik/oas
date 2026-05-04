(** Result monadic binding operators.

    Open this module in files that use [let*] and [let+] for
    {!Result}-based computation chains.

    {2 Usage}

    {[
    open Result_syntax

    let compute x =
      let* a = parse x in
      let* b = validate a in
      let+ c = transform b in
      Ok c
    ]}

    @since 0.187.7
    @stability Stable *)

val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
(** Alias for {!Result.bind}. Sequential bind. *)

val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
(** Alias for [Result.map f]. Sequential map. *)

val ( and* ) : ('a, 'e) result -> ('b, 'e) result -> (('a * 'b), 'e) result
(** Parallel accumulate — both must succeed. *)

val ( and+ ) : ('a, 'e) result -> ('b, 'e) result -> (('a * 'b), 'e) result
(** Parallel accumulate — both must succeed. *)
