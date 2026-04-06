open Runtime

type execution_resolution = {
  selected_provider: string;
  requested_model: string option;
  resolved_provider: string option;
  resolved_model: string option;
  provider_cfg: Provider.config option;
}

let provider_runtime_name selected (cfg : Provider.config option) =
  match cfg with
  | None -> selected
  | Some cfg -> (
      match cfg.provider with
      | Provider.Local _ -> "local"
      | Provider.Anthropic -> "anthropic"
      | Provider.OpenAICompat _ -> "openai-compat"
      | Provider.Custom_registered { name } -> "custom:" ^ name)

let resolve_provider ?provider ?model () =
  let selected =
    match provider with
    | Some value when String.trim value <> "" -> String.lowercase_ascii (String.trim value)
    | _ -> Defaults.fallback_provider
  in
  let base =
    match selected with
    | "mock" | "echo" -> Ok None
    | "local" | "local-qwen" -> Ok (Some (Provider.local_llm ()))
    | "sonnet" -> Ok (Some (Provider.anthropic_sonnet ()))
    | "haiku" -> Ok (Some (Provider.anthropic_haiku ()))
    | "opus" -> Ok (Some (Provider.anthropic_opus ()))
    | "openrouter" -> Ok (Some (Provider.openrouter ()))
    | other ->
        Error (Error.Config
          (Error.UnsupportedProvider
            { detail = Printf.sprintf "unknown provider %S; valid: local, \
              local-qwen, sonnet, haiku, opus, openrouter, mock, echo" other }))
  in
  match base with
  | Error _ as e -> e
  | Ok None -> Ok None
  | Ok (Some cfg) ->
      Ok (Some
        {
          cfg with
          model_id =
            (match model with Some value when String.trim value <> "" -> value | _ -> cfg.model_id);
        })

let resolve_execution (session : session) (detail : spawn_agent_request) =
  let first_some = Util.first_some in
  let selected_provider =
    match detail.provider with
    | Some value when String.trim value <> "" ->
        String.lowercase_ascii (String.trim value)
    | _ -> (
        match session.provider with
        | Some value when String.trim value <> "" ->
            String.lowercase_ascii (String.trim value)
        | _ -> Defaults.fallback_provider)
  in
  let requested_model =
    match detail.model with
    | Some value when String.trim value <> "" -> Some (String.trim value)
    | _ -> None
  in
  match selected_provider with
  | "mock" | "echo" ->
      Ok {
        selected_provider;
        requested_model;
        resolved_provider = Some selected_provider;
        resolved_model = first_some requested_model session.model;
        provider_cfg = None;
      }
  | _ ->
      match resolve_provider ~provider:selected_provider
              ?model:(first_some requested_model session.model) () with
      | Error _ as e -> e
      | Ok provider_cfg ->
        Ok {
          selected_provider;
          requested_model;
          resolved_provider =
            Some (provider_runtime_name selected_provider provider_cfg);
          resolved_model =
            (match provider_cfg with
            | Some cfg -> Some cfg.model_id
            | None -> first_some requested_model session.model);
          provider_cfg;
        }

[@@@coverage off]
(* === Inline tests === *)

(* --- provider_runtime_name --- *)

let%test "provider_runtime_name: None cfg returns selected" =
  provider_runtime_name "my-provider" None = "my-provider"

let%test "provider_runtime_name: Local provider" =
  let cfg = Some { Provider.provider = Local { base_url = Llm_provider.Constants.Endpoints.default_url_localhost };
                   model_id = "test"; api_key_env = "K" } in
  provider_runtime_name "local" cfg = "local"

let%test "provider_runtime_name: Anthropic provider" =
  let cfg = Some { Provider.provider = Anthropic;
                   model_id = "claude-sonnet-4-6"; api_key_env = "K" } in
  provider_runtime_name "anthropic" cfg = "anthropic"

let%test "provider_runtime_name: OpenAICompat provider" =
  let cfg = Some { Provider.provider = OpenAICompat {
    base_url = "https://api.openai.com"; auth_header = None;
    path = "/v1/chat/completions"; static_token = None };
    model_id = "gpt-4o"; api_key_env = "K" } in
  provider_runtime_name "openai" cfg = "openai-compat"

let%test "provider_runtime_name: Custom_registered provider" =
  let cfg = Some { Provider.provider = Custom_registered { name = "myvendor" };
                   model_id = "test"; api_key_env = "K" } in
  provider_runtime_name "custom" cfg = "custom:myvendor"

(* --- resolve_provider --- *)

let%test "resolve_provider: mock returns Ok None" =
  resolve_provider ~provider:"mock" () = Ok None

let%test "resolve_provider: echo returns Ok None" =
  resolve_provider ~provider:"echo" () = Ok None

let%test "resolve_provider: local returns Local provider" =
  match resolve_provider ~provider:"local" () with
  | Ok (Some cfg) -> (match cfg.provider with Provider.Local _ -> true | _ -> false)
  | _ -> false

let%test "resolve_provider: local-qwen returns Local provider" =
  match resolve_provider ~provider:"local-qwen" () with
  | Ok (Some cfg) -> (match cfg.provider with Provider.Local _ -> true | _ -> false)
  | _ -> false

let%test "resolve_provider: sonnet returns Anthropic" =
  match resolve_provider ~provider:"sonnet" () with
  | Ok (Some cfg) -> cfg.provider = Provider.Anthropic
  | _ -> false

let%test "resolve_provider: haiku returns Anthropic" =
  match resolve_provider ~provider:"haiku" () with
  | Ok (Some cfg) -> cfg.provider = Provider.Anthropic
  | _ -> false

let%test "resolve_provider: opus returns Anthropic" =
  match resolve_provider ~provider:"opus" () with
  | Ok (Some cfg) -> cfg.provider = Provider.Anthropic
  | _ -> false

let%test "resolve_provider: openrouter returns OpenAICompat" =
  match resolve_provider ~provider:"openrouter" () with
  | Ok (Some cfg) -> (match cfg.provider with Provider.OpenAICompat _ -> true | _ -> false)
  | _ -> false

let%test "resolve_provider: unknown returns UnsupportedProvider error" =
  match resolve_provider ~provider:"unknown-provider" () with
  | Error (Error.Config (Error.UnsupportedProvider _)) -> true
  | _ -> false

let%test "resolve_provider: typoed provider returns error" =
  match resolve_provider ~provider:"openriauter" () with
  | Error (Error.Config (Error.UnsupportedProvider _)) -> true
  | _ -> false

let%test "resolve_provider: empty provider uses fallback" =
  (* Empty string triggers fallback_provider *)
  match resolve_provider ~provider:"  " () with
  | Ok (Some _) -> true
  | _ -> false

let%test "resolve_provider: model override applied" =
  match resolve_provider ~provider:"sonnet" ~model:"my-custom-model" () with
  | Ok (Some cfg) -> cfg.model_id = "my-custom-model"
  | _ -> false

let%test "resolve_provider: empty model uses default" =
  match resolve_provider ~provider:"sonnet" ~model:"  " () with
  | Ok (Some cfg) -> cfg.model_id <> ""
  | _ -> false

let%test "resolve_provider: trimmed and lowercased" =
  match resolve_provider ~provider:"  MOCK  " () with
  | Ok None -> true  (* mock returns Ok None *)
  | _ -> false

(* --- resolve_execution --- *)

let dummy_session : Runtime.session = {
  session_id = "test-sess";
  goal = "test";
  title = None;
  tag = None;
  permission_mode = None;
  phase = Running;
  created_at = 0.0;
  updated_at = 0.0;
  provider = None;
  model = None;
  system_prompt = None;
  max_turns = 10;
  workdir = None;
  planned_participants = [];
  participants = [];
  artifacts = [];
  votes = [];
  turn_count = 0;
  last_seq = 0;
  outcome = None;
}

let dummy_spawn : Runtime.spawn_agent_request = {
  participant_name = "agent-1";
  role = Some "execute";
  prompt = "do something";
  provider = None;
  model = None;
  system_prompt = None;
  max_turns = None;
}

let%test "resolve_execution: mock provider has no provider_cfg" =
  let detail = { dummy_spawn with provider = Some "mock" } in
  match resolve_execution dummy_session detail with
  | Ok res ->
    res.selected_provider = "mock"
    && res.provider_cfg = None
    && res.resolved_provider = Some "mock"
  | Error _ -> false

let%test "resolve_execution: echo provider has no provider_cfg" =
  let detail = { dummy_spawn with provider = Some "echo" } in
  match resolve_execution dummy_session detail with
  | Ok res -> res.selected_provider = "echo" && res.provider_cfg = None
  | Error _ -> false

let%test "resolve_execution: detail provider takes priority over session" =
  let session = { dummy_session with provider = Some "sonnet" } in
  let detail = { dummy_spawn with provider = Some "mock" } in
  match resolve_execution session detail with
  | Ok res -> res.selected_provider = "mock"
  | Error _ -> false

let%test "resolve_execution: session provider used when detail is None" =
  let session = { dummy_session with provider = Some "haiku" } in
  match resolve_execution session dummy_spawn with
  | Ok res -> res.selected_provider = "haiku"
  | Error _ -> false

let%test "resolve_execution: fallback when both None" =
  match resolve_execution dummy_session dummy_spawn with
  | Ok res -> res.selected_provider = Defaults.fallback_provider
  | Error _ -> false

let%test "resolve_execution: requested_model from detail" =
  let detail = { dummy_spawn with model = Some "my-model" } in
  match resolve_execution dummy_session detail with
  | Ok res -> res.requested_model = Some "my-model"
  | Error _ -> false

let%test "resolve_execution: requested_model None when detail empty" =
  let detail = { dummy_spawn with model = Some "  " } in
  match resolve_execution dummy_session detail with
  | Ok res -> res.requested_model = None
  | Error _ -> false

let%test "resolve_execution: non-mock has provider_cfg" =
  let detail = { dummy_spawn with provider = Some "sonnet" } in
  match resolve_execution dummy_session detail with
  | Ok res -> res.provider_cfg <> None && res.resolved_provider = Some "anthropic"
  | Error _ -> false

let%test "resolve_execution: unknown provider returns error" =
  let detail = { dummy_spawn with provider = Some "bogus-provider" } in
  match resolve_execution dummy_session detail with
  | Error (Error.Config (Error.UnsupportedProvider _)) -> true
  | _ -> false

let%test "resolve_execution: mock resolved_model uses session model" =
  let session = { dummy_session with model = Some "sess-model" } in
  let detail = { dummy_spawn with provider = Some "mock" } in
  match resolve_execution session detail with
  | Ok res -> res.resolved_model = Some "sess-model"
  | Error _ -> false

let%test "resolve_execution: mock requested_model takes priority" =
  let session = { dummy_session with model = Some "sess-model" } in
  let detail = { dummy_spawn with provider = Some "mock"; model = Some "req-model" } in
  match resolve_execution session detail with
  | Ok res -> res.resolved_model = Some "req-model"
  | Error _ -> false
