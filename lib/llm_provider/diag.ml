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

let debug_enabled =
  match Sys.getenv_opt "OAS_LLM_PROVIDER_DEBUG" with
  | Some ("1" | "true") -> true
  | _ ->
    (* Backward compat: cascade_executor used OAS_CASCADE_DIAG *)
    (match Sys.getenv_opt "OAS_CASCADE_DIAG" with
     | Some ("1" | "true") -> true
     | _ -> false)
;;

let default_sink (lvl : level) ~ctx msg =
  match lvl with
  | Debug when not debug_enabled -> ()
  | _ -> Printf.eprintf "[llm_provider] [%s] [%s] %s\n%!" (level_to_string lvl) ctx msg
;;

let _sink : (level -> ctx:string -> string -> unit) Atomic.t = Atomic.make default_sink
let set_sink s = Atomic.set _sink s
let emit lvl ctx fmt = Printf.ksprintf (fun msg -> (Atomic.get _sink) lvl ~ctx msg) fmt
let debug ctx fmt = emit Debug ctx fmt
let info ctx fmt = emit Info ctx fmt
let warn ctx fmt = emit Warn ctx fmt
let error ctx fmt = emit Error ctx fmt
