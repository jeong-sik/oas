type 'a t =
  { payload : 'a
  ; output_token_receipt : Types.output_token_receipt
  }

let make ~payload ~output_token_receipt = { payload; output_token_receipt }

let map_payload f artifact =
  { payload = f artifact.payload; output_token_receipt = artifact.output_token_receipt }
;;
