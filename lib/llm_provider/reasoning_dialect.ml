(** Provider reasoning/thinking dialect semantics. *)

type toggle_default =
  | Enabled
  | Disabled
  | Provider_default

type toggle_wire =
  | No_toggle
  | Thinking_object of { includes_reasoning_effort : bool }
  | Thinking_object_adaptive
  | Thinking_object_only
  | Chat_template_kwargs
  | Chat_template_token
  | Ollama_think
  | Reasoning_effort
  | Enable_thinking
  | Anthropic_thinking
  | Gemini_thinking_config

type effort_alias_policy =
  | Preserve_effort
  | Deepseek_high_or_max

type sampling_policy =
  { ignored_always : Capabilities.sampling_parameter list
  ; ignored_when_thinking : Capabilities.sampling_parameter list
  }

type replay_policy =
  | No_replay
  | Drop_without_tool_preserve_with_tool
  | Latest_user_turn_tool_calls
  | Preserve_always
  | Provider_hidden_replay

type streaming_reasoning =
  | No_streaming_reasoning
  | Delta_field of string
  | Delta_reasoning_details
  | Template_parser

type output_wire =
  | No_output_control
  | Reasoning_split

type thinking_object_only_control =
  { enabled : bool option
  ; keep_all : bool
  }

type openai_request_wire =
  | Chat_completions
  | Responses

type explicit_enable_encoding =
  | Request_control_field
  | Chat_template_system_token

type explicit_enable_receipt =
  | Explicit_enable_not_requested
  | Explicit_enable_encoded of explicit_enable_encoding
  | Explicit_enable_not_encoded

type request_control_artifact =
  { fields : (string * Yojson.Safe.t) list
  ; explicit_enable_receipt : explicit_enable_receipt
  }

type request_control_rejection =
  | Thinking_budget_unsupported
  | Reasoning_effort_unsupported
  | Reasoning_effort_value_unsupported of Reasoning_effort.t

let request_control_rejection_to_message = function
  | Thinking_budget_unsupported ->
    "Reasoning_dialect.request_control_fields: thinking_budget is unsupported by the \
     selected provider wire"
  | Reasoning_effort_unsupported ->
    "Reasoning_dialect.request_control_fields: reasoning_effort is unsupported by the \
     selected provider wire"
  | Reasoning_effort_value_unsupported effort ->
    Printf.sprintf
      "Reasoning_dialect.request_control_fields: reasoning_effort %S is not supported by \
       the selected provider wire"
      (Reasoning_effort.to_string effort)
;;

type t =
  { toggle_default : toggle_default
  ; toggle_wire : toggle_wire
  ; preserve_wire : Capabilities.preserve_thinking_control_format
  ; effort_alias_policy : effort_alias_policy
  ; sampling_policy : sampling_policy
  ; replay_policy : replay_policy
  ; streaming : streaming_reasoning
  ; output_wire : output_wire
  }

let sampling_supported = { ignored_always = []; ignored_when_thinking = [] }

let default =
  { toggle_default = Provider_default
  ; toggle_wire = No_toggle
  ; preserve_wire = No_preserve_thinking_control
  ; effort_alias_policy = Preserve_effort
  ; sampling_policy = sampling_supported
  ; replay_policy = No_replay
  ; streaming = No_streaming_reasoning
  ; output_wire = No_output_control
  }
;;

let deepseek_ignored_sampling_params =
  [ Capabilities.Temperature
  ; Capabilities.Top_p
  ; Capabilities.Presence_penalty
  ; Capabilities.Frequency_penalty
  ]
;;

let sampling_policy_of_capabilities (caps : Capabilities.capabilities) =
  { sampling_supported with ignored_always = caps.ignored_sampling_parameters }
;;

let base_of_capabilities (caps : Capabilities.capabilities) =
  let preserve_wire = caps.preserve_thinking_control_format in
  let output_wire =
    match caps.reasoning_output_format with
    | No_reasoning_output_format -> No_output_control
    | Split_reasoning_fields -> Reasoning_split
  in
  let default = { default with sampling_policy = sampling_policy_of_capabilities caps } in
  match caps.thinking_control_format with
  | No_thinking_control ->
    let dialect = { default with preserve_wire; output_wire } in
    (match preserve_wire with
     | Always_preserved_thinking ->
       { dialect with
         replay_policy = Preserve_always
       ; streaming = Delta_field "reasoning_content"
       }
     | No_preserve_thinking_control
     | Thinking_object_keep_all
     | Chat_template_kwargs_preserve_thinking
     | Top_level_preserve_thinking -> dialect)
  | Thinking_object ->
    { toggle_default = Enabled
    ; toggle_wire = Thinking_object { includes_reasoning_effort = true }
    ; preserve_wire
    ; effort_alias_policy = Deepseek_high_or_max
    ; sampling_policy =
        { ignored_always = caps.ignored_sampling_parameters
        ; ignored_when_thinking = deepseek_ignored_sampling_params
        }
    ; replay_policy = Drop_without_tool_preserve_with_tool
    ; streaming = Delta_field "reasoning_content"
    ; output_wire
    }
  | Thinking_object_adaptive ->
    { default with
      toggle_default = Enabled
    ; toggle_wire = Thinking_object_adaptive
    ; preserve_wire
    ; streaming =
        (match output_wire with
         | Reasoning_split -> Delta_reasoning_details
         | No_output_control -> Delta_field "reasoning_content")
    ; output_wire
    }
  | Thinking_object_only ->
    let replay_policy =
      match preserve_wire with
      | Thinking_object_keep_all -> Drop_without_tool_preserve_with_tool
      | No_preserve_thinking_control
      | Chat_template_kwargs_preserve_thinking
      | Top_level_preserve_thinking
      | Always_preserved_thinking -> default.replay_policy
    in
    { default with
      toggle_default = Provider_default
    ; toggle_wire = Thinking_object_only
    ; preserve_wire
    ; replay_policy
    ; streaming = Delta_field "reasoning_content"
    ; output_wire
    }
  | Chat_template_kwargs ->
    { default with
      toggle_wire = Chat_template_kwargs
    ; preserve_wire
    ; streaming = Template_parser
    ; output_wire
    }
  | Chat_template_token _ ->
    { default with
      toggle_wire = Chat_template_token
    ; preserve_wire
    ; streaming = Template_parser
    ; output_wire
    }
  | Ollama_think ->
    { default with
      toggle_wire = Ollama_think
    ; preserve_wire
    ; streaming = Delta_field "thinking"
    ; output_wire
    }
  | Reasoning_effort ->
    { default with
      toggle_wire = Reasoning_effort
    ; preserve_wire
    ; streaming = Delta_field "reasoning"
    ; output_wire
    }
  | Enable_thinking ->
    { default with
      toggle_wire = Enable_thinking
    ; preserve_wire
    ; streaming = Delta_field "reasoning_content"
    ; output_wire
    }
;;

let apply_replay_override caps dialect =
  match caps.Capabilities.reasoning_replay_override with
  | Default_reasoning_replay -> dialect
  | Force_no_replay -> { dialect with replay_policy = No_replay }
  | Force_drop_without_tool_preserve_with_tool ->
    { dialect with replay_policy = Drop_without_tool_preserve_with_tool }
  | Force_latest_user_turn_tool_calls ->
    { dialect with replay_policy = Latest_user_turn_tool_calls }
  | Force_preserve_always -> { dialect with replay_policy = Preserve_always }
;;

let apply_streaming_format caps dialect =
  match caps.Capabilities.reasoning_streaming_format with
  | Default_reasoning_streaming -> dialect
  | No_reasoning_streaming -> { dialect with streaming = No_streaming_reasoning }
  | Delta_reasoning_field field -> { dialect with streaming = Delta_field field }
  | Template_reasoning_streaming -> { dialect with streaming = Template_parser }
;;

let of_capabilities caps =
  base_of_capabilities caps |> apply_streaming_format caps |> apply_replay_override caps
;;

let with_preserve_thinking ~preserve_thinking dialect =
  match dialect.preserve_wire, preserve_thinking with
  | Always_preserved_thinking, _ -> { dialect with replay_policy = Preserve_always }
  | ( ( Thinking_object_keep_all
      | Chat_template_kwargs_preserve_thinking
      | Top_level_preserve_thinking )
    , Some true ) -> { dialect with replay_policy = Preserve_always }
  | ( ( No_preserve_thinking_control
      | Thinking_object_keep_all
      | Chat_template_kwargs_preserve_thinking
      | Top_level_preserve_thinking )
    , _ ) -> dialect
;;

let thinking_enabled ~enable_thinking =
  match enable_thinking with
  | Some false -> false
  | Some true | None -> true
;;

let thinking_object_only_control dialect ~enable_thinking ~preserve_thinking =
  let keep_all =
    match dialect.preserve_wire with
    | Thinking_object_keep_all ->
      (match preserve_thinking, enable_thinking with
       | Some true, (None | Some true) -> true
       | Some true, Some false | Some false, _ | None, _ -> false)
    | No_preserve_thinking_control
    | Chat_template_kwargs_preserve_thinking
    | Top_level_preserve_thinking
    | Always_preserved_thinking -> false
  in
  let enabled =
    match enable_thinking with
    | Some _ as enabled -> enabled
    | None when keep_all -> Some true
    | None -> None
  in
  { enabled; keep_all }
;;

let chat_template_kwargs_preserve_field dialect ~preserve_thinking =
  match dialect.preserve_wire with
  | Chat_template_kwargs_preserve_thinking -> preserve_thinking
  | No_preserve_thinking_control
  | Thinking_object_keep_all
  | Top_level_preserve_thinking
  | Always_preserved_thinking -> None
;;

let top_level_preserve_field dialect ~preserve_thinking =
  match dialect.preserve_wire with
  | Top_level_preserve_thinking -> preserve_thinking
  | No_preserve_thinking_control
  | Thinking_object_keep_all
  | Chat_template_kwargs_preserve_thinking
  | Always_preserved_thinking -> None
;;

let ignores_sampling_param dialect ~enable_thinking parameter =
  List.mem parameter dialect.sampling_policy.ignored_always
  || (thinking_enabled ~enable_thinking
      && List.mem parameter dialect.sampling_policy.ignored_when_thinking)
;;

let bool_field name = function
  | Some value -> [ name, `Bool value ]
  | None -> []
;;

let reasoning_output_fields dialect ~enable_thinking =
  match dialect.output_wire, thinking_enabled ~enable_thinking with
  | Reasoning_split, true -> [ "reasoning_split", `Bool true ]
  | Reasoning_split, false | No_output_control, _ -> []
;;

let normalize_effort_value dialect effort =
  match dialect.effort_alias_policy, (effort : Reasoning_effort.t) with
  | Deepseek_high_or_max, Reasoning_effort.High -> Some "high"
  | Deepseek_high_or_max, Reasoning_effort.Max -> Some "max"
  | ( Deepseek_high_or_max
    , ( Reasoning_effort.None_
      | Reasoning_effort.Minimal
      | Reasoning_effort.Low
      | Reasoning_effort.Medium
      | Reasoning_effort.XHigh ) ) -> None
  | Preserve_effort, effort -> Some (Reasoning_effort.to_string effort)
;;

let validate_request_control_inputs
      request_wire
      dialect
      ~thinking_budget
      ~reasoning_effort
  =
  let thinking_budget_result =
    match thinking_budget, request_wire, dialect.toggle_wire with
    | None, _, _ | Some _, Chat_completions, Enable_thinking -> Ok ()
    | Some _, _, _ -> Error Thinking_budget_unsupported
  in
  match thinking_budget_result with
  | Error _ as error -> error
  | Ok () ->
    (match reasoning_effort, request_wire, dialect.toggle_wire with
     | None, _, _
     | Some _, (Chat_completions | Responses), Reasoning_effort
     | Some _, Chat_completions, Thinking_object { includes_reasoning_effort = true } ->
       Ok ()
     | Some _, _, _ -> Error Reasoning_effort_unsupported)
;;

let normalized_effort_for_request dialect = function
  | None -> Ok None
  | Some effort ->
    (match normalize_effort_value dialect effort with
     | Some normalized -> Ok (Some normalized)
     | None -> Error (Reasoning_effort_value_unsupported effort))
;;

let request_control_fields
      request_wire
      dialect
      ~enable_thinking
      ~preserve_thinking
      ~thinking_budget
      ~reasoning_effort
      ?zai_glm_clear_thinking
      ()
  =
  match
    validate_request_control_inputs
      request_wire
      dialect
      ~thinking_budget
      ~reasoning_effort
  with
  | Error _ as error -> error
  | Ok () ->
    (match normalized_effort_for_request dialect reasoning_effort with
     | Error _ as error -> error
     | Ok normalized_effort ->
       let normalized_effort_field () =
         match normalized_effort with
         | Some effort -> [ "reasoning_effort", `String effort ]
         | None -> []
       in
       let explicit_field_encoding =
         match enable_thinking with
         | Some true -> Some Request_control_field
         | Some false | None -> None
       in
       let explicit_reasoning_effort_encoding =
         (* [None_] is the typed no-reasoning setting, not proof that an
            explicit enable request reached the wire. Keep this decision on
            the constructor so admission never interprets the wire string. *)
         match enable_thinking, reasoning_effort with
         | ( Some true
           , Some
               ( Reasoning_effort.Minimal
               | Reasoning_effort.Low
               | Reasoning_effort.Medium
               | Reasoning_effort.High
               | Reasoning_effort.XHigh
               | Reasoning_effort.Max ) ) -> Some Request_control_field
         | Some true, (Some Reasoning_effort.None_ | None) | (Some false | None), _ ->
           None
       in
       let fields, explicit_enable_encoding =
         match request_wire, dialect.toggle_wire with
         | Responses, Reasoning_effort ->
           (match normalized_effort with
            | Some effort ->
              ( [ "reasoning", `Assoc [ "effort", `String effort ] ]
              , explicit_reasoning_effort_encoding )
            | None -> [], None)
         | ( Responses
           , ( No_toggle
             | Thinking_object _
             | Thinking_object_adaptive
             | Thinking_object_only
             | Chat_template_kwargs
             | Chat_template_token
             | Ollama_think
             | Enable_thinking
             | Anthropic_thinking
             | Gemini_thinking_config ) ) -> [], None
         | Chat_completions, Chat_template_kwargs ->
           let fields =
             bool_field "enable_thinking" enable_thinking
             @ bool_field
                 "preserve_thinking"
                 (chat_template_kwargs_preserve_field dialect ~preserve_thinking)
           in
           ( (match fields with
              | [] -> []
              | fields -> [ "chat_template_kwargs", `Assoc fields ])
           , explicit_field_encoding )
         | Chat_completions, Chat_template_token ->
           let encoding =
             match enable_thinking with
             | Some true -> Some Chat_template_system_token
             | Some false | None -> None
           in
           [], encoding
         | Chat_completions, Ollama_think -> [], None
         | Chat_completions, Enable_thinking ->
           let fields =
             bool_field "enable_thinking" enable_thinking
             @ bool_field
                 "preserve_thinking"
                 (top_level_preserve_field dialect ~preserve_thinking)
           in
           let fields =
             match enable_thinking, thinking_budget with
             | Some true, Some budget -> ("thinking_budget", `Int budget) :: fields
             | _ -> fields
           in
           fields, explicit_field_encoding
         | Chat_completions, Reasoning_effort ->
           let fields = normalized_effort_field () in
           fields, explicit_reasoning_effort_encoding
         | Chat_completions, Thinking_object _ ->
           let fields =
             match enable_thinking with
             | Some true ->
               ("thinking", `Assoc [ "type", `String "enabled" ])
               :: normalized_effort_field ()
             | Some false -> [ "thinking", `Assoc [ "type", `String "disabled" ] ]
             | None -> []
           in
           fields, explicit_field_encoding
         | Chat_completions, Thinking_object_adaptive ->
           let fields =
             match enable_thinking with
             | Some true -> [ "thinking", `Assoc [ "type", `String "adaptive" ] ]
             | Some false -> [ "thinking", `Assoc [ "type", `String "disabled" ] ]
             | None -> []
           in
           fields, explicit_field_encoding
         | Chat_completions, Thinking_object_only ->
           let control =
             thinking_object_only_control dialect ~enable_thinking ~preserve_thinking
           in
           let fields =
             match control.enabled with
             | Some enabled ->
               [ "type", `String (if enabled then "enabled" else "disabled") ]
             | None -> []
           in
           let fields =
             if control.keep_all then fields @ [ "keep", `String "all" ] else fields
           in
           ( (match fields with
              | [] -> []
              | fields -> [ "thinking", `Assoc fields ])
           , explicit_field_encoding )
         | Chat_completions, No_toggle ->
           (match zai_glm_clear_thinking, enable_thinking with
            | Some clear_thinking, Some true ->
              ( [ ( "thinking"
                  , `Assoc
                      [ "type", `String "enabled"
                      ; "clear_thinking", `Bool clear_thinking
                      ] )
                ]
              , explicit_field_encoding )
            | Some _, Some false ->
              [ "thinking", `Assoc [ "type", `String "disabled" ] ], None
            | Some _, None | None, _ -> [], None)
         | Chat_completions, (Anthropic_thinking | Gemini_thinking_config) -> [], None
       in
       let output_fields =
         match request_wire with
         | Chat_completions -> reasoning_output_fields dialect ~enable_thinking
         | Responses -> []
       in
       let explicit_enable_receipt =
         match enable_thinking, explicit_enable_encoding with
         | Some true, Some encoding -> Explicit_enable_encoded encoding
         | Some true, None -> Explicit_enable_not_encoded
         | Some false, _ | None, _ -> Explicit_enable_not_requested
       in
       Ok { fields = output_fields @ fields; explicit_enable_receipt })
;;

let provider_capabilities_of_kind kind = Capabilities.capabilities_of_kind kind

let base_for_provider_config (config : Provider_config.t) =
  match config.kind with
  | Anthropic ->
    { default with
      toggle_wire = Anthropic_thinking
    ; replay_policy = Preserve_always
    ; streaming = Delta_field "thinking_delta"
    }
  | Gemini ->
    { default with
      toggle_wire = Gemini_thinking_config
    ; (* GenerateContent is stateless. Signed parts must remain attached to the
         exact model response part and be returned unchanged. *)
      replay_policy = Preserve_always
    ; streaming = Delta_field "thought"
    }
  | Kimi | OpenAI_compat | Ollama | Glm ->
    (match Provider_config.capabilities_for_config_model config with
     | Some caps -> of_capabilities caps
     | None -> provider_capabilities_of_kind config.kind |> of_capabilities)
  | DashScope ->
    (* DashScope emits top-level enable_thinking/preserve_thinking regardless of
       the model catalog. *)
    of_capabilities Capabilities.dashscope_capabilities
;;

let for_provider_config (config : Provider_config.t) =
  let dialect =
    base_for_provider_config config
    |> with_preserve_thinking ~preserve_thinking:config.preserve_thinking
  in
  let dialect =
    match config.kind with
    | Kimi | OpenAI_compat | Ollama | Glm ->
      (* RFC-OAS-029 S3.1: GLM reasoning replay is
       clear_thinking-conditional (Preserved Thinking = thinking active AND
       clear_thinking=false). The GLM capability profile carries
       [No_thinking_control]/[No_preserve_thinking_control], so it resolves to
       the default [No_replay] dialect and the typed [replay_policy] is a dead
       value. Resolve the GLM conditional to a typed [replay_policy] here, at
       the single dialect boundary, so the request serializer consumes only the
       typed policy (via [should_replay_reasoning]) instead of re-deriving
       GLM-ness with [is_glm_request]/[glm_should_replay_reasoning] at
       serialize time (S3.1: replay is typed, one source). *)
      if Provider_config.is_zai_glm_config config
      then
        { dialect with
          replay_policy =
            (if Provider_config.glm_should_replay_reasoning config
             then Preserve_always
             else No_replay)
        }
      else dialect
    | Anthropic | Gemini | DashScope -> dialect
  in
  match Provider_http_codec.of_config config with
  | Provider_http_codec.Openai_responses ->
    { dialect with replay_policy = Provider_hidden_replay }
  | Anthropic_messages | Openai_chat | Ollama_chat | Gemini_generate_content | Glm_chat ->
    dialect
;;

let replay_contract dialect : Reasoning_replay_contract.t =
  let replay_policy =
    match dialect.replay_policy with
    | No_replay -> Reasoning_replay_contract.No_replay
    | Drop_without_tool_preserve_with_tool -> Tool_call_assistant_messages_all_history
    | Latest_user_turn_tool_calls -> Tool_call_assistant_messages_latest_user_turn
    | Preserve_always -> All_assistant_messages
    | Provider_hidden_replay -> Provider_opaque_state
  in
  let streaming =
    match dialect.streaming with
    | No_streaming_reasoning -> Reasoning_replay_contract.No_streaming_reasoning
    | Delta_field field -> Delta_field field
    | Delta_reasoning_details -> Delta_reasoning_details
    | Template_parser -> Template_parser
  in
  let output_wire =
    match dialect.output_wire with
    | No_output_control -> Reasoning_replay_contract.No_output_control
    | Reasoning_split -> Reasoning_split
  in
  { replay_policy; streaming; output_wire }
;;

let reasoning_source_for_provider_config config =
  (* Artifact compatibility is derived from the stable provider/model codec.
     Request-local replay selection (for example [preserve_thinking] or GLM
     [clear_thinking]) is applied by [for_provider_config] at the consuming
     boundary and must not rewrite the identity of an already-produced
     artifact. The HTTP codec is part of the stable shape, hence the explicit
     Responses override below. *)
  let dialect = base_for_provider_config config in
  let dialect =
    match Provider_http_codec.of_config config with
    | Provider_http_codec.Openai_responses ->
      { dialect with replay_policy = Provider_hidden_replay }
    | Anthropic_messages | Openai_chat | Ollama_chat | Gemini_generate_content | Glm_chat
      -> dialect
  in
  Types.Reasoning_source.create
    ~provider_kind:config.Provider_config.kind
    ~provider_instance:
      (Types.Reasoning_source.provider_instance
         ~base_url:config.base_url
         ~request_path:config.request_path)
    ~canonical_model_id:config.model_id
    ~replay_contract:(replay_contract dialect)
;;

let sampling_params_ignored_when_thinking dialect =
  let params =
    dialect.sampling_policy.ignored_always @ dialect.sampling_policy.ignored_when_thinking
  in
  List.fold_left
    (fun acc parameter -> if List.mem parameter acc then acc else acc @ [ parameter ])
    []
    params
;;

(* Sampling params a wire format ignores while thinking is enabled, keyed purely
   by the format so both request builders can consult it without a full dialect.
   Only DeepSeek-style [Thinking_object] suppresses sampling; the constant is the
   single source of truth, also used by [of_capabilities] above. *)
let sampling_params_ignored_for_format
  : Capabilities.thinking_control_format -> Capabilities.sampling_parameter list
  = function
  | Capabilities.Thinking_object -> deepseek_ignored_sampling_params
  | Capabilities.No_thinking_control
  | Capabilities.Thinking_object_adaptive
  | Capabilities.Thinking_object_only
  | Capabilities.Chat_template_kwargs
  | Capabilities.Chat_template_token _
  | Capabilities.Ollama_think
  | Capabilities.Reasoning_effort
  | Capabilities.Enable_thinking -> []
;;

let sampling_field_ignored_when_thinking
      ~thinking_control_format
      ~enable_thinking
      ~parameter
  =
  let thinking_active =
    match enable_thinking with
    | Some false -> false
    | Some true | None -> true
  in
  thinking_active
  && List.mem parameter (sampling_params_ignored_for_format thinking_control_format)
;;

let should_replay_reasoning dialect ~assistant_had_tool_call =
  match dialect.replay_policy with
  | No_replay | Provider_hidden_replay -> false
  | Preserve_always -> true
  | Drop_without_tool_preserve_with_tool | Latest_user_turn_tool_calls ->
    assistant_had_tool_call
;;

let requires_reasoning_replay_on_tool_call dialect =
  should_replay_reasoning dialect ~assistant_had_tool_call:true
  && not (should_replay_reasoning dialect ~assistant_had_tool_call:false)
;;

let toggle_wire_to_string = function
  | No_toggle -> "no_toggle"
  | Thinking_object { includes_reasoning_effort = true } -> "thinking_object"
  | Thinking_object { includes_reasoning_effort = false } -> "thinking_object_no_effort"
  | Thinking_object_adaptive -> "thinking_object_adaptive"
  | Thinking_object_only -> "thinking_object_only"
  | Chat_template_kwargs -> "chat_template_kwargs"
  | Chat_template_token -> "chat_template_token"
  | Ollama_think -> "ollama_think"
  | Reasoning_effort -> "reasoning_effort"
  | Enable_thinking -> "enable_thinking"
  | Anthropic_thinking -> "anthropic_thinking"
  | Gemini_thinking_config -> "gemini_thinking_config"
;;

let replay_policy_to_string = function
  | No_replay -> "no_replay"
  | Drop_without_tool_preserve_with_tool -> "drop_without_tool_preserve_with_tool"
  | Latest_user_turn_tool_calls -> "latest_user_turn_tool_calls"
  | Preserve_always -> "preserve_always"
  | Provider_hidden_replay -> "provider_hidden_replay"
;;

[@@@coverage off]

let%test "reasoning_replay_override Force_preserve_always lifts base no_replay" =
  let caps =
    { Capabilities.default_capabilities with
      supports_reasoning = true
    ; thinking_control_format = Capabilities.Reasoning_effort
    ; reasoning_replay_override = Capabilities.Force_preserve_always
    }
  in
  let dialect = of_capabilities caps in
  (* base Reasoning_effort yields No_replay; the override lifts it to Preserve_always
     so reasoning is replayed on both plain and tool turns. *)
  should_replay_reasoning dialect ~assistant_had_tool_call:false
  && should_replay_reasoning dialect ~assistant_had_tool_call:true
;;

let%test "reasoning_replay_override drop_without_tool replays only on tool turns" =
  let caps =
    { Capabilities.default_capabilities with
      supports_reasoning = true
    ; thinking_control_format = Capabilities.Reasoning_effort
    ; reasoning_replay_override = Capabilities.Force_drop_without_tool_preserve_with_tool
    }
  in
  let dialect = of_capabilities caps in
  (not (should_replay_reasoning dialect ~assistant_had_tool_call:false))
  && should_replay_reasoning dialect ~assistant_had_tool_call:true
;;

let%test "reasoning_replay_override latest_user_turn stays structurally distinct" =
  let caps =
    { Capabilities.default_capabilities with
      supports_reasoning = true
    ; thinking_control_format = Capabilities.Chat_template_kwargs
    ; reasoning_replay_override = Capabilities.Force_latest_user_turn_tool_calls
    }
  in
  let dialect = of_capabilities caps in
  replay_policy_to_string dialect.replay_policy = "latest_user_turn_tool_calls"
  && should_replay_reasoning dialect ~assistant_had_tool_call:true
  && not (should_replay_reasoning dialect ~assistant_had_tool_call:false)
;;

let%test
    "reasoning_replay_override default keeps base policy (Reasoning_effort = no_replay)"
  =
  let caps =
    { Capabilities.default_capabilities with
      supports_reasoning = true
    ; thinking_control_format = Capabilities.Reasoning_effort
    }
  in
  not (should_replay_reasoning (of_capabilities caps) ~assistant_had_tool_call:false)
;;

let%test
    "kimi base profile replays reasoning on every turn (live path: bare kimi-k2.* -> \
     base=kimi)"
  =
  (* A consumer may send a bare API name (e.g. "kimi-k2.6"). Longest-prefix
     matching resolves it to the native "kimi-k2" catalog row whose base="kimi",
     so this profile is the dialect applied on that path. Kimi requires
     reasoning replay (hard-required on tool turns, recommended always). The
     Kimi base profile now carries both Always_preserved_thinking and
     Force_preserve_always so catalog inheritance keeps replay explicit. Revert
     that override -> both arms go false. *)
  let dialect = of_capabilities Capabilities.kimi_capabilities in
  should_replay_reasoning dialect ~assistant_had_tool_call:false
  && should_replay_reasoning dialect ~assistant_had_tool_call:true
;;

let%test "request_control_fields emits qwen chat_template kwargs" =
  let dialect =
    of_capabilities
      { Capabilities.default_capabilities with
        supports_reasoning = true
      ; thinking_control_format = Capabilities.Chat_template_kwargs
      ; preserve_thinking_control_format =
          Capabilities.Chat_template_kwargs_preserve_thinking
      }
  in
  request_control_fields
    Chat_completions
    dialect
    ~enable_thinking:(Some false)
    ~preserve_thinking:(Some true)
    ~thinking_budget:None
    ~reasoning_effort:None
    ()
  = Ok
      { fields =
          [ ( "chat_template_kwargs"
            , `Assoc [ "enable_thinking", `Bool false; "preserve_thinking", `Bool true ] )
          ]
      ; explicit_enable_receipt = Explicit_enable_not_requested
      }
;;

let%test "request_control_fields emits thinking object with explicitly supported effort" =
  let dialect =
    of_capabilities
      { Capabilities.default_capabilities with
        supports_reasoning = true
      ; thinking_control_format = Capabilities.Thinking_object
      }
  in
  request_control_fields
    Chat_completions
    dialect
    ~enable_thinking:(Some true)
    ~preserve_thinking:None
    ~thinking_budget:None
    ~reasoning_effort:(Some Reasoning_effort.High)
    ()
  = Ok
      { fields =
          [ "thinking", `Assoc [ "type", `String "enabled" ]
          ; "reasoning_effort", `String "high"
          ]
      ; explicit_enable_receipt = Explicit_enable_encoded Request_control_field
      }
;;

let%test "request_control_fields keeps zai glm no-toggle exception explicit" =
  request_control_fields
    Chat_completions
    default
    ~enable_thinking:(Some true)
    ~preserve_thinking:(Some true)
    ~thinking_budget:None
    ~reasoning_effort:None
    ~zai_glm_clear_thinking:false
    ()
  = Ok
      { fields =
          [ ( "thinking"
            , `Assoc [ "type", `String "enabled"; "clear_thinking", `Bool false ] )
          ]
      ; explicit_enable_receipt = Explicit_enable_encoded Request_control_field
      }
;;

let%test "explicit enable receipt follows the selected OpenAI request wire" =
  let dialect format =
    of_capabilities
      { Capabilities.default_capabilities with
        supports_reasoning = true
      ; thinking_control_format = format
      }
  in
  let request request_wire dialect ?reasoning_effort () =
    request_control_fields
      request_wire
      dialect
      ~enable_thinking:(Some true)
      ~preserve_thinking:None
      ~thinking_budget:None
      ~reasoning_effort
      ()
  in
  let reasoning_effort_dialect = dialect Capabilities.Reasoning_effort in
  let thinking_object_dialect = dialect Capabilities.Thinking_object in
  request Chat_completions reasoning_effort_dialect ()
  = Ok { fields = []; explicit_enable_receipt = Explicit_enable_not_encoded }
  && request
       Responses
       reasoning_effort_dialect
       ~reasoning_effort:Reasoning_effort.Medium
       ()
     = Ok
         { fields = [ "reasoning", `Assoc [ "effort", `String "medium" ] ]
         ; explicit_enable_receipt = Explicit_enable_encoded Request_control_field
         }
  && request
       Responses
       reasoning_effort_dialect
       ~reasoning_effort:Reasoning_effort.None_
       ()
     = Ok
         { fields = [ "reasoning", `Assoc [ "effort", `String "none" ] ]
         ; explicit_enable_receipt = Explicit_enable_not_encoded
         }
  && request Responses thinking_object_dialect ()
     = Ok { fields = []; explicit_enable_receipt = Explicit_enable_not_encoded }
;;

let%test "chat-template token receipt names its out-of-band wire encoding" =
  let dialect =
    of_capabilities
      { Capabilities.default_capabilities with
        supports_reasoning = true
      ; thinking_control_format = Capabilities.Chat_template_token "<THINK>"
      }
  in
  request_control_fields
    Chat_completions
    dialect
    ~enable_thinking:(Some true)
    ~preserve_thinking:None
    ~thinking_budget:None
    ~reasoning_effort:None
    ()
  = Ok
      { fields = []
      ; explicit_enable_receipt = Explicit_enable_encoded Chat_template_system_token
      }
;;

let%test "unrelated output field cannot satisfy explicit enable receipt" =
  let dialect =
    of_capabilities
      { Capabilities.default_capabilities with
        supports_reasoning = true
      ; thinking_control_format = Capabilities.Ollama_think
      ; reasoning_output_format = Capabilities.Split_reasoning_fields
      }
  in
  request_control_fields
    Chat_completions
    dialect
    ~enable_thinking:(Some true)
    ~preserve_thinking:None
    ~thinking_budget:None
    ~reasoning_effort:None
    ()
  = Ok
      { fields = [ "reasoning_split", `Bool true ]
      ; explicit_enable_receipt = Explicit_enable_not_encoded
      }
;;
