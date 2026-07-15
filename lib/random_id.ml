let hex_digit value =
  if value < 10
  then Char.chr (Char.code '0' + value)
  else Char.chr (Char.code 'a' + value - 10)
;;

let hex_of_string value =
  let encoded = Bytes.create (String.length value * 2) in
  String.iteri
    (fun index byte ->
       let value = Char.code byte in
       Bytes.set encoded (index * 2) (hex_digit (value lsr 4));
       Bytes.set encoded ((index * 2) + 1) (hex_digit (value land 0x0f)))
    value;
  Bytes.unsafe_to_string encoded
;;

let create () =
  try Ok (Mirage_crypto_rng_unix.getrandom 16 |> hex_of_string) with
  | exn ->
    Llm_provider.Reserved_exn.reraise_if_reserved exn;
    Error ("operating-system entropy unavailable: " ^ Printexc.to_string exn)
;;
