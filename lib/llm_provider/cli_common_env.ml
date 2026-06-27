let trim_non_empty s =
  let trimmed = String.trim s in
  if trimmed = "" then None else Some trimmed
;;

let trim_non_empty_opt = function
  | None -> None
  | Some s -> trim_non_empty s
;;

let get name = trim_non_empty_opt (Sys.getenv_opt name)

let bool name =
  match get name with
  | None -> false
  | Some v -> Env_parse.bool_of_string v
;;

let filter_non_empty = List.filter (fun s -> s <> "")

let split_on_char_trim sep s =
  String.split_on_char sep s |> List.map String.trim |> filter_non_empty
;;

let list ?(sep = ',') name =
  (* Treat unset, empty, and whitespace-only as the same "no value"
     signal (all → None).  OCaml [Unix.putenv k ""] cannot truly unset
     a variable, which would otherwise leak "set to empty = disable
     all" semantics across tests.  Callers wanting an explicit
     "disable all" should use a dedicated boolean env var instead. *)
  match get name with
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

let kv_pairs name =
  match get name with
  | None -> None
  | Some v -> Some (split_on_char_trim ',' v |> List.filter_map parse_kv)
;;

let int ?(allow_negative = false) ~default var =
  match Sys.getenv_opt var with
  | Some raw ->
    let trimmed = String.trim raw in
    if trimmed = ""
    then default
    else (
      match int_of_string_opt trimmed with
      | Some v when allow_negative || v >= 0 -> v
      | Some v ->
        Diag.warn
          "cli_common_env"
          "%s=%S is negative (%d); using default %d"
          var
          raw
          v
          default;
        default
      | None ->
        Diag.warn
          "cli_common_env"
          "%s=%S is not an integer; using default %d"
          var
          raw
          default;
        default)
  | None -> default
;;

[@@@coverage off]

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
            level = Diag.Warn && ctx = "cli_common_env" && String.contains msg '-')
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
            level = Diag.Warn && ctx = "cli_common_env" && String.contains msg 'n')
         !warnings)
;;
