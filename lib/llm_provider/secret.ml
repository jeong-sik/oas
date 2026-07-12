(** Minimal abstract secret holder.

    [t] is kept abstract (but coercible to [string] via the private type in
    the interface) so that API keys and tokens cannot be accidentally
    interpolated into logs or JSON.  The only intentional escape hatches are
    {!header_value} for HTTP header construction and the coercion in tests.

    @since 0.207.0 *)

type t = string
type identity = Identity of string

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
let digest s = Digestif.SHA256.(to_hex (digest_string s))
let identity s = if is_empty s then None else Some (Identity (digest s))
let equal_identity (Identity left) (Identity right) = String.equal left right
let hash_identity (Identity digest) = Hashtbl.hash digest

let identity_fingerprint (Identity digest) =
  if String.length digest >= 8 then String.sub digest 0 8 else digest
;;

let fingerprint s =
  let hex = digest s in
  if String.length hex >= 8 then String.sub hex 0 8 else hex
;;

let%test "fingerprint is stable" =
  fingerprint (of_string "my-secret-key") = fingerprint (of_string "my-secret-key")
;;

let%test "fingerprint differs for different secrets" =
  fingerprint (of_string "a") <> fingerprint (of_string "b")
;;

let%test "empty secret is empty" = is_empty empty
let%test "empty secret has no identity" = Option.is_none (identity empty)

let%test "secret identity is stable and opaque" =
  match identity (of_string "my-secret-key"), identity (of_string "my-secret-key") with
  | Some left, Some right ->
    equal_identity left right
    && hash_identity left = hash_identity right
    && String.equal (identity_fingerprint left) (fingerprint (of_string "my-secret-key"))
  | None, _ | _, None -> false
;;

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
