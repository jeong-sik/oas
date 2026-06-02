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

let env_bool name =
  match Sys.getenv_opt name with
  | None -> false
  | Some raw ->
    (match String.lowercase_ascii (String.trim raw) with
     | "1" | "true" | "yes" | "on" -> true
     | _ -> false)
;;

let debug_enabled = env_bool "OAS_LLM_PROVIDER_DEBUG" || env_bool "OAS_CASCADE_DIAG"

let default_sink (lvl : level) ~ctx msg =
  match lvl with
  | Debug when not debug_enabled -> ()
  | _ -> Printf.eprintf "[llm_provider] [%s] [%s] %s\n%!" (level_to_string lvl) ctx msg
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
