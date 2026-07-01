(** Ollama native API request building and response parsing.

    Uses [/api/chat] endpoint with [think] parameter for thinking control
    except token-controlled models whose documented thinking control is
    chat-template based,
    and [options] object for sampling parameters.

    @since 0.113.0 *)

open Types

let ( let* ) = Result.bind
let keep_alive_env_var = "OAS_OLLAMA_KEEP_ALIVE"

(* ── Request building ────────────────────────────────── *)

let with_chat_template_thinking_token ~token = function
  | Some prompt when not (Api_common.string_is_blank prompt) ->
    let trimmed = String.trim prompt in
    if String.starts_with ~prefix:token trimmed then trimmed else token ^ "\n" ^ trimmed
  | _ -> token
;;

let chat_template_thinking_token_exn ~(config : Provider_config.t) =
  match Provider_config.thinking_control_token_for_config_model config with
  | Some token -> token
  | None ->
    invalid_arg
      (Printf.sprintf
         "Backend_ollama.build_request: model %S declares chat_template_token \
          thinking_control_format but no thinking_control_token"
         config.model_id)
;;

(** Build Ollama native [/api/chat] request body.
    Key differences from Openai compat:
    - [think] parameter (boolean) instead of [chat_template_kwargs], except
      token-controlled models where thinking is triggered by a catalog-declared
      token in the system prompt
    - Sampling params go inside [options] object
    - [num_predict] instead of [max_tokens]
    - No [tool_choice] support *)
let build_request
      ?(stream = false)
      ~(config : Provider_config.t)
      ~(messages : message list)
      ?(tools : Yojson.Safe.t list = [])
      ()
  =
  let think_requested =
    match config.enable_thinking with
    | Some true -> true
    | Some false -> false
    | None -> Cli_common_env.bool "OAS_OLLAMA_THINK_DEFAULT"
  in
  let caps =
    match Provider_config.capabilities_for_config_model config with
    | Some c -> c
    | None -> Capabilities.ollama_capabilities
  in
  let chat_template_token_thinking =
    think_requested && caps.thinking_control_format = Capabilities.Chat_template_token
  in
  let system_prompt =
    if chat_template_token_thinking
    then (
      let token = chat_template_thinking_token_exn ~config in
      Some (with_chat_template_thinking_token ~token config.system_prompt))
    else config.system_prompt
  in
  let messages = Tool_message_pairs.close_for_provider_request messages in
  let provider_messages =
    (match system_prompt with
     | Some s when not (Api_common.string_is_blank s) ->
       [ `Assoc
           [ "role", `String "system"; "content", `String (Utf8_sanitize.sanitize s) ]
       ]
     | _ -> [])
    @ List.concat_map
        (Backend_openai_serialize.ollama_messages_of_message ~model_id:config.model_id)
        messages
  in
  let body = [ "model", `String config.model_id; "messages", `List provider_messages ] in
  (* think: false by default for Ollama to prevent thinking models from
     consuming all tokens in reasoning. Only enable when explicitly requested.
     Override with OAS_OLLAMA_THINK_DEFAULT=true to enable thinking for all
     Ollama requests by default.

     Some token-controlled models are the exception: current Ollama rejects
     [think:true] even though the model's documented control is a
     chat-template token. For those catalog-declared rows we inject the token
     into the system turn and omit the top-level [think] field so Ollama returns
     [message.thinking] instead of failing the request. *)
  let body =
    if chat_template_token_thinking
    then body
    else ("think", `Bool think_requested) :: body
  in
  (* Ollama defaults to stream=true, so always send explicit value *)
  let body = ("stream", `Bool stream) :: body in
  (* keep_alive: controls how long the model stays loaded in memory after
     the request. Ollama's default is 5 minutes, which causes models to be
     unloaded between automated cycles and re-loaded on demand — slow and
     eviction-prone when other processes ping different models.

     We default to -1 (permanent) so the caller's pinned model stays
     resident. Resolution order:
     1. [config.keep_alive] (routing profile / per-call override; the
        first-class SSOT a TOML editor reaches for)
     2. [OAS_OLLAMA_KEEP_ALIVE] env var (operator-wide default)
     3. SDK default ["-1"] (permanent residency)
     Accepted values:
     - integer seconds: "-1", "0", "3600" → sent as [`Int n]
     - duration strings: "5m", "30m", "24h", "-1m" → sent as [`String v]

     Wire format matters: Ollama parses [keep_alive] in two ways depending
     on JSON type. Integer [-1] is the documented sentinel for "keep
     forever" and is always accepted. A string value goes through Go's
     [time.ParseDuration], which requires a unit suffix — [time.ParseDuration "-1"]
     fails with "missing unit in duration". Sending the plain string "-1"
     therefore produces [Invalid request: time: missing unit in duration "-1"]
     and every automated turn errors out in <2s.

     Empirical rationale:
     - 2026-04-11 incident 1: a 35b-a3b model was evicted ~every 30 min
       because each consumer turn reset keep_alive to the 5m default.
     - 2026-04-11 incident 2: after pinning keep_alive=-1 (PR #813), every
       ollama request failed with the duration parse error above because
       -1 was serialized as [`String "-1"]. This fix sends an integer for
       parseable values and a duration string otherwise. *)
  let keep_alive_raw =
    match Cli_common_env.trim_non_empty_opt config.keep_alive with
    | Some v -> v
    | None ->
      (match Cli_common_env.get keep_alive_env_var with
       | Some v -> v
       | None -> "-1")
  in
  let keep_alive_json : Yojson.Safe.t =
    match int_of_string_opt keep_alive_raw with
    | Some n -> `Int n
    | None -> `String keep_alive_raw
  in
  let body = ("keep_alive", keep_alive_json) :: body in
  let body =
    match config.output_schema, config.response_format with
    | Some schema, _ -> ("format", schema) :: body
    | None, Types.JsonSchema schema -> ("format", schema) :: body
    | None, Types.JsonMode -> ("format", `String "json") :: body
    | None, Types.Off -> body
  in
  let body =
    match tools with
    | [] -> body
    | ts ->
      ("tools", `List (List.map Backend_openai_serialize.build_openai_tool_json ts))
      :: body
  in
  (* Sampling parameters go inside Ollama's "options" object.

     top_k / min_p are now capability-gated — not because native
     Ollama rejects them (its Options struct has both, llama.cpp
     samplers support them), but to mirror the #830/#831 contract
     across every OAS serializer so a future capability record that
     lowers either flag actually takes effect everywhere in the
     request-build pipeline.

     For the default ollama_capabilities (inherited from
     openai_compat_chat_extended_capabilities) both flags are true, so
     behaviour is byte-identical for the common path. The gate only
     fires when an operator explicitly sets [supports_min_p = false]
     or [supports_top_k = false] for a specific Ollama variant — at
     which point the one-shot WARN from Backend_openai also fires. *)
  let options = ref [] in
  (let mt =
     Option.value
       ~default:(Constants.resolve_unknown_model_max_tokens_fallback ())
       config.max_tokens
   in
   options := ("num_predict", `Int mt) :: !options);
  (match config.temperature with
   | Some t -> options := ("temperature", `Float t) :: !options
   | None -> ());
  (match config.top_p with
   | Some p -> options := ("top_p", `Float p) :: !options
   | None -> ());
  (match config.top_k with
   | Some k when caps.supports_top_k -> options := ("top_k", `Int k) :: !options
   | Some _ ->
     Backend_openai.warn_capability_drop ~model_id:config.model_id ~field:"top_k"
   | None -> ());
  (match config.min_p with
   | Some p when caps.supports_min_p -> options := ("min_p", `Float p) :: !options
   | Some _ ->
     Backend_openai.warn_capability_drop ~model_id:config.model_id ~field:"min_p"
   | None -> ());
  (* num_ctx: per-request KV cache allocation in tokens. Honored by Ollama
     only. [None] omits the field so Ollama uses its own default
     (Modelfile-declared or 4096). Profile-level setting; non-positive
     values are treated as "unset" to keep profile authoring forgiving. *)
  (match config.num_ctx with
   | Some n when n > 0 -> options := ("num_ctx", `Int n) :: !options
   | _ -> ());
  let body = ("options", `Assoc !options) :: body in
  Yojson.Safe.to_string (`Assoc body)
;;

(* ── Response parsing ────────────────────────────────── *)

let parse_ollama_tool_arguments ~tool_index json =
  match json with
  | `Assoc _ as input -> Ok input
  | `String s when not (Api_common.string_is_blank s) ->
    (match Yojson.Safe.from_string s with
     | `Assoc _ as input -> Ok input
     | `Null | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ ->
       Error
         (Printf.sprintf
            "malformed_ollama_tool_call_arguments:index:%d:not_object"
            tool_index)
     | exception Yojson.Json_error msg ->
       Error
         (Printf.sprintf
            "malformed_ollama_tool_call_arguments:index:%d:%s"
            tool_index
            msg))
  | `Null | `String _ ->
    Error
      (Printf.sprintf "malformed_ollama_tool_call:index:%d:missing_arguments" tool_index)
  | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ ->
    Error
      (Printf.sprintf
         "malformed_ollama_tool_call_arguments:index:%d:not_object"
         tool_index)
;;

let parse_ollama_tool_call ~tool_index tc =
  let open Yojson.Safe.Util in
  let* fn =
    match tc |> member "function" with
    | `Assoc _ as fn -> Ok fn
    | `Null | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ ->
      Error
        (Printf.sprintf "malformed_ollama_tool_call:index:%d:missing_function" tool_index)
  in
  let* name =
    match fn |> member "name" |> to_string_option with
    | Some name when not (Api_common.string_is_blank name) -> Ok name
    | Some _ | None ->
      Error (Printf.sprintf "malformed_ollama_tool_call:index:%d:missing_name" tool_index)
  in
  let* input = parse_ollama_tool_arguments ~tool_index (fn |> member "arguments") in
  let synthetic_id =
    Printf.sprintf "%s_%d" (Api_common.synthesize_tool_use_id ~name input) tool_index
  in
  let id =
    match tc |> member "id" |> to_string_option with
    | Some id when not (Api_common.string_is_blank id) -> id
    | Some _ | None -> synthetic_id
  in
  Ok (ToolUse { id; name; input })
;;

let parse_ollama_tool_calls = function
  | `Null -> Ok []
  | `List calls ->
    let rec loop acc index = function
      | [] -> Ok (List.rev acc)
      | tc :: rest ->
        (match parse_ollama_tool_call ~tool_index:index tc with
         | Ok tool_call -> loop (tool_call :: acc) (index + 1) rest
         | Error _ as error -> error)
    in
    loop [] 0 calls
  | `Assoc _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ ->
    Error "malformed_ollama_tool_calls:not_list"
;;

let parse_ollama_response json_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string json_str in
  match json |> member "error" with
  | `String s -> Error s
  | `Assoc _ as err -> Error (Yojson.Safe.to_string err)
  | _ ->
    let message = json |> member "message" in
    let* text_content, tool_blocks, thinking_blocks =
      match message with
      | `Assoc _ ->
        let txt =
          match message |> member "content" with
          | `String s -> s
          | _ -> ""
        in
        let* tools = parse_ollama_tool_calls (message |> member "tool_calls") in
        if List.length tools > 1
        then
          Diag.debug
            "backend_ollama"
            "parsed %d Ollama tool_calls from one assistant response"
            (List.length tools);
        let thinking =
          match message |> member "thinking" with
          | `String s when not (Api_common.string_is_blank s) ->
            [ Thinking { signature = None; content = s } ]
          | _ -> []
        in
        Ok (txt, tools, thinking)
      | _ -> Ok ("", [], [])
    in
    let done_reason =
      json |> member "done_reason" |> to_string_option |> Option.value ~default:"stop"
    in
    let stop_reason =
      match String.lowercase_ascii done_reason with
      | "tool_calls" when tool_blocks <> [] -> StopToolUse
      | "length" -> MaxTokens
      | "stop" -> EndTurn
      | _other when tool_blocks <> [] -> StopToolUse
      | other -> Unknown other
    in
    let input_tokens = Cli_common_json.member_int "prompt_eval_count" json in
    let output_tokens = Cli_common_json.member_int "eval_count" json in
    let usage =
      if input_tokens = 0 && output_tokens = 0
      then None
      else
        Some
          { input_tokens
          ; output_tokens
          ; cache_creation_input_tokens = 0
          ; cache_read_input_tokens = 0
          ; cost_usd = None
          }
    in
    let telemetry =
      let system_fingerprint = None in
      (* Ollama reports durations in nanoseconds. Surface them as
       inference_timings so downstream can distinguish hardware
       decode rate from wall-clock tok/s. *)
      let timings =
        let prompt_n = json |> member "prompt_eval_count" |> to_int_option in
        let prompt_ns = json |> member "prompt_eval_duration" |> to_int_option in
        let predicted_n = json |> member "eval_count" |> to_int_option in
        let predicted_ns = json |> member "eval_duration" |> to_int_option in
        let any_set =
          Option.is_some prompt_n
          || Option.is_some prompt_ns
          || Option.is_some predicted_n
          || Option.is_some predicted_ns
        in
        if not any_set
        then None
        else (
          let ms_of_ns ns_opt = Option.map (fun ns -> float_of_int ns /. 1e6) ns_opt in
          let per_second n_opt ns_opt =
            match n_opt, ns_opt with
            | Some n, Some ns when ns > 0 ->
              Some (float_of_int n /. (float_of_int ns /. 1e9))
            | _ -> None
          in
          Some
            { Types.prompt_n
            ; prompt_ms = ms_of_ns prompt_ns
            ; prompt_per_second = per_second prompt_n prompt_ns
            ; predicted_n
            ; predicted_ms = ms_of_ns predicted_ns
            ; predicted_per_second = per_second predicted_n predicted_ns
            ; cache_n = None
            })
      in
      let reasoning_tokens = None in
      Some
        { Types.default_inference_telemetry with
          system_fingerprint
        ; timings
        ; reasoning_tokens
        }
    in
    Ok
      { id = json |> member "model" |> to_string_option |> Option.value ~default:"ollama"
      ; model = Cli_common_json.member_str "model" json
      ; stop_reason
      ; content =
          thinking_blocks
          @ (if Api_common.string_is_blank text_content then [] else [ Text text_content ])
          @ tool_blocks
      ; usage
      ; telemetry
      }
;;

(* ── Inline tests ────────────────────────────────── *)

[@@@coverage off]

(** Run [f] with the [OAS_OLLAMA_KEEP_ALIVE] env var set to [value], then
    restore the caller's original setting. Guaranteed restore on exception.
    OCaml's [Unix]/[Sys] modules have no portable unsetenv (verified against
    the installed 5.4.1 Unix.mli/Sys.mli — neither declares one), so a var
    that was originally unset is restored to [""] rather than truly removed
    from the process environment. This is equivalent for every current
    reader ([Cli_common_env.get] treats [Some ""] the same as [None] via
    [trim_non_empty_opt]), but is not a true unset for code that reads the
    var via bare [Sys.getenv_opt]/[Cli_common_env.default_getenv]. *)
let with_keep_alive_env value f =
  let orig = Cli_common_env.default_getenv keep_alive_env_var in
  let restore () =
    match orig with
    | None -> Unix.putenv keep_alive_env_var ""
    | Some v -> Unix.putenv keep_alive_env_var v
  in
  Fun.protect ~finally:restore (fun () ->
    Unix.putenv keep_alive_env_var value;
    f ())
;;

let%test "build_request pins keep_alive=-1 as integer by default" =
  with_keep_alive_env "" (fun () ->
    let config =
      Provider_config.make
        ~kind:Ollama
        ~model_id:"dashscope-3.5:35b-a3b-nvfp4"
        ~base_url:"http://127.0.0.1:11434"
        ()
    in
    let messages =
      [ { role = User
        ; content = [ Text "hi" ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ]
    in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    (* Integer wire format: -1 as [`Int (-1)] avoids Ollama's
       [time.ParseDuration "-1"] failure ("missing unit in duration"). *)
    json |> member "keep_alive" |> to_int = -1)
;;

let%test "build_request integer override sent as `Int" =
  with_keep_alive_env "3600" (fun () ->
    let config =
      Provider_config.make
        ~kind:Ollama
        ~model_id:"dashscope-3.5:35b-a3b-nvfp4"
        ~base_url:"http://127.0.0.1:11434"
        ()
    in
    let messages =
      [ { role = User
        ; content = [ Text "hi" ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ]
    in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "keep_alive" |> to_int = 3600)
;;

let%test "build_request duration string override sent as `String" =
  with_keep_alive_env "30m" (fun () ->
    let config =
      Provider_config.make
        ~kind:Ollama
        ~model_id:"dashscope-3.5:35b-a3b-nvfp4"
        ~base_url:"http://127.0.0.1:11434"
        ()
    in
    let messages =
      [ { role = User
        ; content = [ Text "hi" ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ]
    in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "keep_alive" |> to_string = "30m")
;;

let%test "build_request trims whitespace around override" =
  with_keep_alive_env "  -1m  " (fun () ->
    let config =
      Provider_config.make
        ~kind:Ollama
        ~model_id:"dashscope-3.5:35b-a3b-nvfp4"
        ~base_url:"http://127.0.0.1:11434"
        ()
    in
    let messages =
      [ { role = User
        ; content = [ Text "hi" ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ]
    in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "keep_alive" |> to_string = "-1m")
;;

let%test "build_request whitespace-only env falls back to default integer" =
  with_keep_alive_env "   " (fun () ->
    let config =
      Provider_config.make
        ~kind:Ollama
        ~model_id:"dashscope-3.5:35b-a3b-nvfp4"
        ~base_url:"http://127.0.0.1:11434"
        ()
    in
    let messages =
      [ { role = User
        ; content = [ Text "hi" ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ]
    in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "keep_alive" |> to_int = -1)
;;

let%test "build_request config.keep_alive overrides env (string form)" =
  with_keep_alive_env "-1" (fun () ->
    let config =
      Provider_config.make
        ~kind:Ollama
        ~model_id:"dashscope-3.5:35b-a3b-nvfp4"
        ~base_url:"http://127.0.0.1:11434"
        ~keep_alive:"5m"
        ()
    in
    let messages =
      [ { role = User
        ; content = [ Text "hi" ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ]
    in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "keep_alive" |> to_string = "5m")
;;

let%test "build_request config.keep_alive integer form sent as `Int" =
  with_keep_alive_env "" (fun () ->
    let config =
      Provider_config.make
        ~kind:Ollama
        ~model_id:"dashscope-3.5:35b-a3b-nvfp4"
        ~base_url:"http://127.0.0.1:11434"
        ~keep_alive:"600"
        ()
    in
    let messages =
      [ { role = User
        ; content = [ Text "hi" ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ]
    in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "keep_alive" |> to_int = 600)
;;

let%test "build_request config.num_ctx injected into options" =
  with_keep_alive_env "" (fun () ->
    let config =
      Provider_config.make
        ~kind:Ollama
        ~model_id:"dashscope-3:8b"
        ~base_url:"http://127.0.0.1:11434"
        ~num_ctx:8192
        ()
    in
    let messages =
      [ { role = User
        ; content = [ Text "hi" ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ]
    in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "options" |> member "num_ctx" |> to_int = 8192)
;;

let%test "build_request omits num_ctx when None" =
  with_keep_alive_env "" (fun () ->
    let config =
      Provider_config.make
        ~kind:Ollama
        ~model_id:"dashscope-3:8b"
        ~base_url:"http://127.0.0.1:11434"
        ()
    in
    let messages =
      [ { role = User
        ; content = [ Text "hi" ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ]
    in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "options" |> member "num_ctx" = `Null)
;;

let%test "build_request num_ctx<=0 treated as unset" =
  with_keep_alive_env "" (fun () ->
    let config =
      Provider_config.make
        ~kind:Ollama
        ~model_id:"dashscope-3:8b"
        ~base_url:"http://127.0.0.1:11434"
        ~num_ctx:0
        ()
    in
    let messages =
      [ { role = User
        ; content = [ Text "hi" ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ]
    in
    let body = build_request ~config ~messages () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "options" |> member "num_ctx" = `Null)
;;

let%test "parse_ollama_response populates timings from eval_count/eval_duration" =
  let json =
    {|{"model":"dashscope-3.5:35b-a3b-nvfp4","done":true,"done_reason":"stop",
       "message":{"role":"assistant","content":"hi"},
       "prompt_eval_count":100,"prompt_eval_duration":200000000,
       "eval_count":120,"eval_duration":2000000000}|}
  in
  match parse_ollama_response json with
  | Error _ -> false
  | Ok resp ->
    (match resp.telemetry with
     | Some { timings = Some t; _ } ->
       t.predicted_n = Some 120
       && (match t.predicted_per_second with
           | Some v -> abs_float (v -. 60.0) < 0.001
           | None -> false)
       && t.prompt_n = Some 100
       &&
         (match t.prompt_per_second with
         | Some v -> abs_float (v -. 500.0) < 0.001
         | None -> false)
     | _ -> false)
;;

let%test "parse_ollama_response maps prompt/eval counts to usage" =
  let json =
    {|{"model":"dashscope-3.5:35b-a3b-nvfp4","done":true,"done_reason":"stop",
       "message":{"role":"assistant","content":"hi"},
       "prompt_eval_count":17,"eval_count":23}|}
  in
  match parse_ollama_response json with
  | Error _ -> false
  | Ok resp ->
    (match resp.usage with
     | Some usage ->
       usage.input_tokens = 17
       && usage.output_tokens = 23
       && usage.cache_creation_input_tokens = 0
       && usage.cache_read_input_tokens = 0
     | None -> false)
;;

let%test "parse_ollama_response guards zero eval_duration" =
  let json =
    {|{"model":"dashscope-3.5:35b-a3b-nvfp4","done":true,"done_reason":"stop",
       "message":{"role":"assistant","content":"hi"},
       "eval_count":10,"eval_duration":0}|}
  in
  match parse_ollama_response json with
  | Error _ -> false
  | Ok resp ->
    (match resp.telemetry with
     | Some { timings = Some t; _ } ->
       (* eval_count present → timings record exists,
          but predicted_per_second is None because duration is 0. *)
       t.predicted_n = Some 10 && t.predicted_per_second = None
     | _ -> false)
;;

let%test "parse_ollama_response returns timings=None when no timing fields present" =
  let json =
    {|{"model":"dashscope-3.5:35b-a3b-nvfp4","done":true,"done_reason":"stop",
       "message":{"role":"assistant","content":"hi"}}|}
  in
  match parse_ollama_response json with
  | Error _ -> false
  | Ok resp ->
    (match resp.telemetry with
     | Some { timings = None; _ } -> true
     | _ -> false)
;;

let%test "build_request sets think=true when enable_thinking=true" =
  with_keep_alive_env "" (fun () ->
    let config =
      Provider_config.make
        ~kind:Ollama
        ~model_id:"dashscope-3:8b"
        ~base_url:"http://127.0.0.1:11434"
        ~enable_thinking:true
        ()
    in
    let body = build_request ~config ~messages:[] () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "think" |> to_bool = true)
;;

let%test "build_request sets think=false when enable_thinking=false" =
  with_keep_alive_env "" (fun () ->
    let config =
      Provider_config.make
        ~kind:Ollama
        ~model_id:"dashscope-3:8b"
        ~base_url:"http://127.0.0.1:11434"
        ~enable_thinking:false
        ()
    in
    let body = build_request ~config ~messages:[] () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "think" |> to_bool = false)
;;

let%test "build_request maps max_tokens to num_predict in options" =
  with_keep_alive_env "" (fun () ->
    let config =
      Provider_config.make
        ~kind:Ollama
        ~model_id:"dashscope-3:8b"
        ~base_url:"http://127.0.0.1:11434"
        ~max_tokens:2048
        ()
    in
    let body = build_request ~config ~messages:[] () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "options" |> member "num_predict" |> to_int = 2048)
;;

let%test
    "build_request includes top_k in options when supports_top_k=true (default ollama \
     caps)"
  =
  (* Pins the supported path: default Ollama capabilities have
     supports_top_k=true, so the serializer threads top_k into options.
     The capability-gated drop path (supports_top_k=false ->
     warn_capability_drop) is covered by the OpenAI-compat tests in
     backend_openai.ml; Ollama uses the same gate via shared helpers. *)
  with_keep_alive_env "" (fun () ->
    let config =
      Provider_config.make
        ~kind:Ollama
        ~model_id:"dashscope-3:8b"
        ~base_url:"http://127.0.0.1:11434"
        ~top_k:40
        ()
    in
    let body = build_request ~config ~messages:[] () in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "options" |> member "top_k" |> to_int = 40)
;;

let%test "parse_ollama_response maps done_reason=tool_calls to StopToolUse" =
  let json =
    {|{"model":"dashscope-3:8b","done":true,"done_reason":"tool_calls",
       "message":{"role":"assistant","content":"",
         "tool_calls":[{"function":{"name":"get_weather","arguments":"{\"city\":\"Seoul\"}"}}]}}|}
  in
  match parse_ollama_response json with
  | Error _ -> false
  | Ok resp ->
    resp.stop_reason = Types.StopToolUse
    &&
      (match resp.content with
      | [ Types.ToolUse { name = "get_weather"; _ } ] -> true
      | _ -> false)
;;

let%test "parse_ollama_response returns Error on error field" =
  let json = {|{"error":"model \"nonexistent\" not found, try pulling it first"}|} in
  match parse_ollama_response json with
  | Error msg -> String.starts_with ~prefix:"model" msg
  | Ok _ -> false
;;

let%test "parse_ollama_response extracts thinking block from message" =
  let json =
    {|{"model":"dashscope-3:8b","done":true,"done_reason":"stop",
       "message":{"role":"assistant","content":"The answer is 42.",
         "thinking":"Let me reason about this step by step."}}|}
  in
  match parse_ollama_response json with
  | Error _ -> false
  | Ok resp ->
    (match resp.content with
     | [ Types.Thinking { content = thinking }; Types.Text "The answer is 42." ] ->
       String.length thinking > 0
     | _ -> false)
;;
