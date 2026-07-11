(** Immutable provider request payload paired with the exact output-token
    decision used to construct it. *)

type 'a t = private
  { payload : 'a
  ; output_token_receipt : Types.output_token_receipt
  }

val make : payload:'a -> output_token_receipt:Types.output_token_receipt -> 'a t
val map_payload : ('a -> 'b) -> 'a t -> 'b t
val payload : 'a t -> 'a
val output_token_receipt : 'a t -> Types.output_token_receipt
