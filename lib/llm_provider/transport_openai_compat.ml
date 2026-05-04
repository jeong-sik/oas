(** OpenAI Chat Completions HTTP transport.

    @since 0.86.0 *)

type config =
  { base_url : string
  ; api_key : string
  ; model_id : string
  ; request_path : string
  ; max_tokens : int
  ; extra_headers : (string * string) list
  }

let default_config =
  { base_url = Discovery.default_endpoint
  ; api_key = ""
  ; model_id = ""
  ; request_path = "/v1/chat/completions"
  ; max_tokens = Constants.Inference.unknown_model_max_tokens_fallback
  ; extra_headers = []
  }
;;

(** Merge transport config with per-request Provider_config.t.
    Per-request fields (temperature, system_prompt, tools, etc.) are
    preserved. Transport config provides base_url, api_key, model_id,
    and headers.

    Note: Provider_config.make fills in defaults for request_path
    and headers, so those fields are never truly empty. Transport
    extra_headers are always appended to request headers. *)
let merge_config ~(transport_cfg : config) (req_cfg : Provider_config.t)
  : Provider_config.t
  =
  let base_url =
    if req_cfg.base_url <> "" then req_cfg.base_url else transport_cfg.base_url
  in
  let api_key =
    if req_cfg.api_key <> "" then req_cfg.api_key else transport_cfg.api_key
  in
  let model_id =
    if req_cfg.model_id <> "" then req_cfg.model_id else transport_cfg.model_id
  in
  let request_path =
    if req_cfg.request_path <> ""
    then req_cfg.request_path
    else transport_cfg.request_path
  in
  let max_tokens =
    match req_cfg.max_tokens with
    | Some n when n > 0 -> Some n
    | _ -> Some transport_cfg.max_tokens
  in
  (* Always append transport extra_headers after request headers. *)
  let headers = req_cfg.headers @ transport_cfg.extra_headers in
  { req_cfg with
    kind = Provider_config.OpenAI_compat
  ; base_url
  ; api_key
  ; model_id
  ; request_path
  ; max_tokens
  ; headers
  }
;;

let create ~sw ~net ~(config : config) : Llm_transport.t =
  let http_transport = Complete.make_http_transport ~sw ~net in
  { complete_sync =
      (fun (req : Llm_transport.completion_request) ->
        let merged = merge_config ~transport_cfg:config req.config in
        http_transport.complete_sync { req with config = merged })
  ; complete_stream =
      (fun ~on_event (req : Llm_transport.completion_request) ->
        let merged = merge_config ~transport_cfg:config req.config in
        http_transport.complete_stream ~on_event { req with config = merged })
  }
;;

(* ── Inline tests ────────────────────────────────────── *)

[@@@coverage off]

let%test "default_config base_url" =
  default_config.base_url = Constants.Endpoints.default_url
;;

let%test "default_config request_path" =
  default_config.request_path = "/v1/chat/completions"
;;

let%test "default_config max_tokens" =
  default_config.max_tokens = Constants.Inference.unknown_model_max_tokens_fallback
;;

let%test "default_config api_key empty" = default_config.api_key = ""

let%test "merge_config uses transport base_url when req empty" =
  let transport_cfg = { default_config with base_url = "http://myserver:9000" } in
  let req_cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"" ~base_url:"" () in
  let merged = merge_config ~transport_cfg req_cfg in
  merged.base_url = "http://myserver:9000"
;;

let%test "merge_config preserves req base_url when present" =
  let transport_cfg = { default_config with base_url = "http://myserver:9000" } in
  let req_cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:""
      ~base_url:"http://override:8080"
      ()
  in
  let merged = merge_config ~transport_cfg req_cfg in
  merged.base_url = "http://override:8080"
;;

let%test "merge_config uses transport model_id when req empty" =
  let transport_cfg = { default_config with model_id = "qwen3.5-35b" } in
  let req_cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"" ~base_url:"" () in
  let merged = merge_config ~transport_cfg req_cfg in
  merged.model_id = "qwen3.5-35b"
;;

let%test "merge_config preserves req model_id when present" =
  let transport_cfg = { default_config with model_id = "qwen3.5-35b" } in
  let req_cfg =
    Provider_config.make ~kind:OpenAI_compat ~model_id:"gpt-4o" ~base_url:"" ()
  in
  let merged = merge_config ~transport_cfg req_cfg in
  merged.model_id = "gpt-4o"
;;

let%test "merge_config uses transport api_key when req empty" =
  let transport_cfg = { default_config with api_key = "sk-test123" } in
  let req_cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"" () in
  let merged = merge_config ~transport_cfg req_cfg in
  merged.api_key = "sk-test123"
;;

let%test "merge_config preserves req request_path (make fills default)" =
  let transport_cfg = { default_config with request_path = "/api/v2/chat" } in
  let req_cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"" () in
  let merged = merge_config ~transport_cfg req_cfg in
  (* Provider_config.make fills request_path = "/v1/chat/completions" for OpenAI_compat *)
  merged.request_path = "/v1/chat/completions"
;;

let%test "merge_config uses transport request_path when req explicitly empty" =
  let transport_cfg = { default_config with request_path = "/api/v2/chat" } in
  let req_cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:""
      ~request_path:""
      ()
  in
  let merged = merge_config ~transport_cfg req_cfg in
  merged.request_path = "/api/v2/chat"
;;

let%test "merge_config sets kind to OpenAI_compat regardless" =
  let transport_cfg = default_config in
  let req_cfg = Provider_config.make ~kind:Anthropic ~model_id:"" ~base_url:"" () in
  let merged = merge_config ~transport_cfg req_cfg in
  merged.kind = Provider_config.OpenAI_compat
;;

let%test "merge_config appends extra_headers to explicit req headers" =
  let transport_cfg = { default_config with extra_headers = [ "X-Custom", "value" ] } in
  let req_cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:""
      ~headers:[ "Authorization", "Bearer sk" ]
      ()
  in
  let merged = merge_config ~transport_cfg req_cfg in
  List.length merged.headers = 2
  && List.mem ("Authorization", "Bearer sk") merged.headers
  && List.mem ("X-Custom", "value") merged.headers
;;

let%test "merge_config appends extra_headers to default req headers" =
  let transport_cfg = { default_config with extra_headers = [ "X-Custom", "value" ] } in
  let req_cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"" () in
  let merged = merge_config ~transport_cfg req_cfg in
  (* Provider_config.make defaults headers to [("Content-Type", "application/json")] *)
  List.length merged.headers = 2
  && List.mem ("Content-Type", "application/json") merged.headers
  && List.mem ("X-Custom", "value") merged.headers
;;

let%test "merge_config no extra_headers preserves req headers" =
  let transport_cfg = { default_config with extra_headers = [] } in
  let req_cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"" () in
  let merged = merge_config ~transport_cfg req_cfg in
  merged.headers = [ "Content-Type", "application/json" ]
;;

let%test "merge_config uses transport max_tokens when req zero" =
  let transport_cfg = { default_config with max_tokens = 8192 } in
  let req_cfg =
    Provider_config.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"" ~max_tokens:0 ()
  in
  let merged = merge_config ~transport_cfg req_cfg in
  merged.max_tokens = Some 8192
;;

let%test "merge_config preserves req max_tokens when positive" =
  let transport_cfg = { default_config with max_tokens = 8192 } in
  let req_cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:""
      ~max_tokens:2048
      ()
  in
  let merged = merge_config ~transport_cfg req_cfg in
  merged.max_tokens = Some 2048
;;

let%test "merge_config preserves req temperature" =
  let transport_cfg = default_config in
  let req_cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:""
      ~temperature:0.7
      ()
  in
  let merged = merge_config ~transport_cfg req_cfg in
  merged.temperature = Some 0.7
;;

let%test "merge_config preserves req system_prompt" =
  let transport_cfg = default_config in
  let req_cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:""
      ~system_prompt:"Be helpful."
      ()
  in
  let merged = merge_config ~transport_cfg req_cfg in
  merged.system_prompt = Some "Be helpful."
;;
