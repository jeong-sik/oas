type level =
  | Debug
  | Info
  | Warn
  | Error

let level_to_string = function
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warn -> "WARN"
  | Error -> "ERROR"
;;

let debug_enabled () =
  Env_parse.bool_env "OAS_LLM_PROVIDER_DEBUG" || Env_parse.bool_env "OAS_CASCADE_DIAG"
;;

let default_sink (lvl : level) ~ctx msg =
  match lvl with
  | Debug when not (debug_enabled ()) -> ()
  | _ ->
    Printf.eprintf
      "[llm_provider] [%s] [%s] %s\n%!"
      (level_to_string lvl)
      ctx
      (Secret_redactor.redact_string msg)
;;

let _sink : (level -> ctx:string -> string -> unit) Atomic.t = Atomic.make default_sink
let set_sink s = Atomic.set _sink s

let with_sink sink f =
  let previous = Atomic.get _sink in
  Atomic.set _sink sink;
  Fun.protect ~finally:(fun () -> Atomic.set _sink previous) f
;;

let emit lvl ctx fmt = Printf.ksprintf (fun msg -> (Atomic.get _sink) lvl ~ctx msg) fmt
let debug ctx fmt = emit Debug ctx fmt
let info ctx fmt = emit Info ctx fmt
let warn ctx fmt = emit Warn ctx fmt
let error ctx fmt = emit Error ctx fmt

let%test "debug_enabled reads OAS_LLM_PROVIDER_DEBUG at call time" =
  Env_parse.with_env "OAS_CASCADE_DIAG" "" (fun () ->
    Env_parse.with_env "OAS_LLM_PROVIDER_DEBUG" "" (fun () ->
      (not (debug_enabled ()))
      &&
      (Unix.putenv "OAS_LLM_PROVIDER_DEBUG" "1";
       debug_enabled ())))
;;

let%test "debug_enabled reads OAS_CASCADE_DIAG alias at call time" =
  Env_parse.with_env "OAS_LLM_PROVIDER_DEBUG" "" (fun () ->
    Env_parse.with_env "OAS_CASCADE_DIAG" "" (fun () ->
      (not (debug_enabled ()))
      &&
      (Unix.putenv "OAS_CASCADE_DIAG" "true";
       debug_enabled ())))
;;
