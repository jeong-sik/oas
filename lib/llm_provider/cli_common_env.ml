let trim_non_empty s =
  let trimmed = String.trim s in
  if trimmed = "" then None else Some trimmed
;;

let trim_non_empty_opt = function
  | None -> None
  | Some s -> trim_non_empty s
;;

let default_getenv = Sys.getenv_opt

(* [get ?getenv name] is the canonical env-read primitive. The optional
   [getenv] argument (default [Sys.getenv_opt]) is a dependency-injection
   seam so callers and tests can resolve the environment without touching
   the process env (RFC-OAS-024 §6 cut 5). The pure core never calls this
   directly — it receives resolved config values as arguments. *)
let get ?(getenv = default_getenv) name = trim_non_empty_opt (getenv name)

type invalid_env =
  { var : string
  ; raw : string
  ; expected : string
  }

let warn_invalid ~on_invalid ~var ~raw ~expected ~diag =
  match on_invalid with
  | Some f -> f { var; raw; expected }
  | None -> diag ()
;;

let bool ?(getenv = default_getenv) ?(default = false) ?on_invalid name =
  match get ~getenv name with
  | None -> default
  | Some v ->
    (match String.lowercase_ascii (String.trim v) with
     | "1" | "true" | "yes" | "on" | "0" | "false" | "no" | "off" | "" ->
       Env_parse.bool_of_string ~default v
     | _ ->
       warn_invalid ~on_invalid ~var:name ~raw:v ~expected:"boolean" ~diag:(fun () ->
         Diag.warn
           "cli_common_env"
           "%s=%S is not a boolean; using default %b"
           name
           v
           default);
       default)
;;

let filter_non_empty = List.filter (fun s -> s <> "")

let split_on_char_trim sep s =
  String.split_on_char sep s |> List.map String.trim |> filter_non_empty
;;

let list ?(getenv = default_getenv) ?(sep = ',') name =
  (* Treat unset, empty, and whitespace-only as the same "no value"
     signal (all → None).  OCaml [Unix.putenv k ""] cannot truly unset
     a variable, which would otherwise leak "set to empty = disable
     all" semantics across tests.  Callers wanting an explicit
     "disable all" should use a dedicated boolean env var instead. *)
  match get ~getenv name with
  | None -> None
  | Some v -> Some (split_on_char_trim sep v)
;;

let parse_kv entry =
  match String.index_opt entry '=' with
  | None -> None
  | Some i ->
    let k = String.sub entry 0 i |> String.trim in
    let v = String.sub entry (i + 1) (String.length entry - i - 1) |> String.trim in
    if k = "" then None else Some (k, v)
;;

let kv_pairs ?(getenv = default_getenv) name =
  match get ~getenv name with
  | None -> None
  | Some v -> Some (split_on_char_trim ',' v |> List.filter_map parse_kv)
;;

let int ?(getenv = default_getenv) ?(allow_negative = false) ?on_invalid ~default var =
  let expected = if allow_negative then "integer" else "non-negative integer" in
  match get ~getenv var with
  | None -> default
  | Some raw ->
    (match int_of_string_opt raw with
     | Some v when v >= 0 || allow_negative -> v
     | Some v ->
       warn_invalid ~on_invalid ~var ~raw ~expected ~diag:(fun () ->
         Diag.warn
           "cli_common_env"
           "%s=%S is negative (%d); using default %d"
           var
           raw
           v
           default);
       default
     | None ->
       warn_invalid ~on_invalid ~var ~raw ~expected ~diag:(fun () ->
         Diag.warn
           "cli_common_env"
           "%s=%S is not an integer; using default %d"
           var
           raw
           default);
       default)
;;

let float ?(getenv = default_getenv) ?(allow_negative = false) ?on_invalid ~default var =
  let expected = if allow_negative then "finite float" else "non-negative finite float" in
  match get ~getenv var with
  | None -> default
  | Some raw ->
    (match float_of_string_opt raw with
     | Some v when Float.is_finite v && (v >= 0.0 || allow_negative) -> v
     | Some v ->
       let kind = if Float.is_finite v then "negative" else "not a finite" in
       warn_invalid ~on_invalid ~var ~raw ~expected ~diag:(fun () ->
         Diag.warn
           "cli_common_env"
           "%s=%S is %s (%f); using default %f"
           var
           raw
           kind
           v
           default);
       default
     | None ->
       warn_invalid ~on_invalid ~var ~raw ~expected ~diag:(fun () ->
         Diag.warn
           "cli_common_env"
           "%s=%S is not a float; using default %f"
           var
           raw
           default);
       default)
;;

[@@@coverage off]

(* Message fragments used by tests; keep in sync with [int], [float], [bool]. *)
let msg_is_negative = "is negative"
let msg_is_not_an_integer = "is not an integer"
let msg_is_not_a_float = "is not a float"
let msg_is_not_a_boolean = "is not a boolean"

let string_contains ~needle haystack =
  needle = ""
  ||
  try
    let (_ : int) = Str.search_forward (Str.regexp_string needle) haystack 0 in
    true
  with
  | Not_found -> false
;;

let with_env = Env_parse.with_env

let%test "int accepts positive env value" =
  with_env "OAS_TEST_CLI_COMMON_ENV_INT_POSITIVE" "12" (fun () ->
    int ~default:7 "OAS_TEST_CLI_COMMON_ENV_INT_POSITIVE" = 12)
;;

let%test "int rejects negative env value by default" =
  with_env "OAS_TEST_CLI_COMMON_ENV_INT_NEGATIVE" "-1" (fun () ->
    let warnings = ref [] in
    let value =
      Diag.with_sink
        (fun level ~ctx msg -> warnings := (level, ctx, msg) :: !warnings)
        (fun () -> int ~default:7 "OAS_TEST_CLI_COMMON_ENV_INT_NEGATIVE")
    in
    value = 7
    && List.exists
         (fun (level, ctx, msg) ->
            level = Diag.Warn
            && ctx = "cli_common_env"
            && string_contains ~needle:msg_is_negative msg)
         !warnings)
;;

let%test "int allows negative env value when requested" =
  with_env "OAS_TEST_CLI_COMMON_ENV_INT_ALLOW_NEGATIVE" "-1" (fun () ->
    int ~allow_negative:true ~default:7 "OAS_TEST_CLI_COMMON_ENV_INT_ALLOW_NEGATIVE" = -1)
;;

let%test "int rejects non-numeric env value" =
  with_env "OAS_TEST_CLI_COMMON_ENV_INT_NON_NUMERIC" "not-a-number" (fun () ->
    let warnings = ref [] in
    let value =
      Diag.with_sink
        (fun level ~ctx msg -> warnings := (level, ctx, msg) :: !warnings)
        (fun () -> int ~default:7 "OAS_TEST_CLI_COMMON_ENV_INT_NON_NUMERIC")
    in
    value = 7
    && List.exists
         (fun (level, ctx, msg) ->
            level = Diag.Warn
            && ctx = "cli_common_env"
            && string_contains ~needle:msg_is_not_an_integer msg)
         !warnings)
;;

let%test "float accepts positive env value" =
  with_env "OAS_TEST_CLI_COMMON_ENV_FLOAT_POSITIVE" "3.14" (fun () ->
    float ~default:7.0 "OAS_TEST_CLI_COMMON_ENV_FLOAT_POSITIVE" = 3.14)
;;

let%test "float rejects negative env value by default" =
  with_env "OAS_TEST_CLI_COMMON_ENV_FLOAT_NEGATIVE" "-2.5" (fun () ->
    let warnings = ref [] in
    let value =
      Diag.with_sink
        (fun level ~ctx msg -> warnings := (level, ctx, msg) :: !warnings)
        (fun () -> float ~default:7.0 "OAS_TEST_CLI_COMMON_ENV_FLOAT_NEGATIVE")
    in
    value = 7.0
    && List.exists
         (fun (level, ctx, msg) ->
            level = Diag.Warn
            && ctx = "cli_common_env"
            && string_contains ~needle:msg_is_negative msg)
         !warnings)
;;

let%test "float allows negative env value when requested" =
  with_env "OAS_TEST_CLI_COMMON_ENV_FLOAT_ALLOW_NEGATIVE" "-2.5" (fun () ->
    float ~allow_negative:true ~default:7.0 "OAS_TEST_CLI_COMMON_ENV_FLOAT_ALLOW_NEGATIVE"
    = -2.5)
;;

let%test "float rejects non-numeric env value" =
  with_env "OAS_TEST_CLI_COMMON_ENV_FLOAT_NON_NUMERIC" "not-a-number" (fun () ->
    let warnings = ref [] in
    let value =
      Diag.with_sink
        (fun level ~ctx msg -> warnings := (level, ctx, msg) :: !warnings)
        (fun () -> float ~default:7.0 "OAS_TEST_CLI_COMMON_ENV_FLOAT_NON_NUMERIC")
    in
    value = 7.0
    && List.exists
         (fun (level, ctx, msg) ->
            level = Diag.Warn
            && ctx = "cli_common_env"
            && string_contains ~needle:msg_is_not_a_float msg)
         !warnings)
;;

let%test "float rejects non-finite env value" =
  with_env "OAS_TEST_CLI_COMMON_ENV_FLOAT_INF" "inf" (fun () ->
    let warnings = ref [] in
    let value =
      Diag.with_sink
        (fun level ~ctx msg -> warnings := (level, ctx, msg) :: !warnings)
        (fun () -> float ~default:7.0 "OAS_TEST_CLI_COMMON_ENV_FLOAT_INF")
    in
    value = 7.0
    && List.exists
         (fun (level, ctx, msg) ->
            level = Diag.Warn
            && ctx = "cli_common_env"
            && string_contains ~needle:"not a finite" msg)
         !warnings)
;;

let%test "bool accepts truthy and falsy env values" =
  with_env "OAS_TEST_CLI_COMMON_ENV_BOOL_TRUE" "on" (fun () ->
    with_env "OAS_TEST_CLI_COMMON_ENV_BOOL_FALSE" "off" (fun () ->
      bool "OAS_TEST_CLI_COMMON_ENV_BOOL_TRUE"
      && not (bool ~default:true "OAS_TEST_CLI_COMMON_ENV_BOOL_FALSE")))
;;

let%test "bool rejects invalid env value with warning" =
  with_env "OAS_TEST_CLI_COMMON_ENV_BOOL_BAD" "maybe" (fun () ->
    let warnings = ref [] in
    let value =
      Diag.with_sink
        (fun level ~ctx msg -> warnings := (level, ctx, msg) :: !warnings)
        (fun () -> bool ~default:true "OAS_TEST_CLI_COMMON_ENV_BOOL_BAD")
    in
    value
    && List.exists
         (fun (level, ctx, msg) ->
            level = Diag.Warn
            && ctx = "cli_common_env"
            && string_contains ~needle:msg_is_not_a_boolean msg)
         !warnings)
;;

(* RFC-OAS-024 §6 cut 5: the ?getenv seam lets tests/callers resolve the
   environment deterministically without [Unix.putenv]. These prove the
   seam is honored for every env-reading function. *)
let%test "get honors injected getenv (RFC-OAS-024 seam)" =
  let never _ = None in
  let always_yes _ = Some "yes" in
  get ~getenv:never "OAS_UNSET_VAR_ZZZ" = None
  && get ~getenv:always_yes "OAS_ANY_VAR_ZZZ" = Some "yes"
;;

let%test "bool honors injected getenv (RFC-OAS-024 seam)" =
  let always_on _ = Some "on" in
  let always_off _ = Some "off" in
  bool ~getenv:always_on "OAS_ANY_VAR_ZZZ"
  && not (bool ~getenv:always_off "OAS_ANY_VAR_ZZZ")
;;

let%test "int honors injected getenv (RFC-OAS-024 seam)" =
  let always_42 _ = Some "42" in
  int ~getenv:always_42 ~default:0 "OAS_ANY_VAR_ZZZ" = 42
;;

let%test "list honors injected getenv (RFC-OAS-024 seam)" =
  let csv _ = Some "a, b ,c" in
  list ~getenv:csv "OAS_ANY_VAR_ZZZ" = Some [ "a"; "b"; "c" ]
;;
