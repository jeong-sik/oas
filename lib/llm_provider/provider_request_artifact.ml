type 'a t =
  { payload : 'a
  ; output_token_receipt : Types.output_token_receipt
  }

let make ~payload ~output_token_receipt = { payload; output_token_receipt }
let payload artifact = artifact.payload
let output_token_receipt artifact = artifact.output_token_receipt
