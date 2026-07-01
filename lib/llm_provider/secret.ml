(** Minimal abstract secret holder.

    [t] is kept abstract (but coercible to [string] via the private type in
    the interface) so that API keys and tokens cannot be accidentally
    interpolated into logs or JSON.  The only intentional escape hatches are
    {!header_value} for HTTP header construction and the coercion in tests.

    @since 0.207.0 *)

type t = string

let of_string s = s

let of_env ?(getenv = Cli_common_env.default_getenv) var =
  match Cli_common_env.get ~getenv var with
  | Some s -> Some (of_string s)
  | None -> None
;;

let empty = ""
let is_empty s = String.trim s = ""
let header_value s = s
let length = String.length

let fingerprint s =
  let hex = Digestif.SHA256.(to_hex (digest_string s)) in
  if String.length hex >= 8 then String.sub hex 0 8 else hex
;;

let%test "fingerprint is stable" =
  fingerprint (of_string "my-secret-key") = fingerprint (of_string "my-secret-key")
;;

let%test "fingerprint differs for different secrets" =
  fingerprint (of_string "a") <> fingerprint (of_string "b")
;;

let%test "empty secret is empty" = is_empty empty

let%test "secret of string round-trips through header_value" =
  header_value (of_string "k") = "k"
;;

let%test "secret of_env honors injected env boundary" =
  let getenv name = if String.equal name "OAS_TEST_SECRET" then Some "  k  " else None in
  match of_env ~getenv "OAS_TEST_SECRET" with
  | Some secret -> header_value secret = "k"
  | None -> false
;;

let%test "secret of_env treats empty env value as absent" =
  let getenv name = if String.equal name "OAS_TEST_SECRET" then Some "   " else None in
  of_env ~getenv "OAS_TEST_SECRET" = None
;;
