(** Thinking-control dialect regression matrix.

    The same user-facing [enable_thinking] knob maps to different provider wire
    formats depending on the model family and serving backend. Keep these
    fixtures together so catalog changes cannot silently move a model family to
    the wrong dialect. *)

module PC = Llm_provider.Provider_config
module BOR = Llm_provider.Backend_openai_request
module BOL = Llm_provider.Backend_ollama
module BAN = Llm_provider.Backend_anthropic
module CAP = Llm_provider.Capabilities
module CM = Llm_provider.Capability_manifest
module RD = Llm_provider.Reasoning_dialect
module RE = Llm_provider.Reasoning_effort
open Alcotest
open Llm_provider.Types
open Yojson.Safe.Util

let json_of_body body = Yojson.Safe.from_string body
let member_is_absent name json = json |> member name = `Null

let with_manifest json f =
  match CM.of_json (Yojson.Safe.from_string json) with
  | Error msg -> fail ("manifest parse failed: " ^ msg)
  | Ok manifest ->
    CM.set_global manifest;
    Fun.protect ~finally:CM.clear_global f
;;

let check_member_absent name json =
  check bool (name ^ " absent") true (member_is_absent name json)
;;

let openai_compat_config ?enable_thinking ?preserve_thinking ?thinking_budget model_id =
  PC.make
    ~kind:OpenAI_compat
    ~model_id
    ~base_url:"https://provider.example/v1"
    ?enable_thinking
    ?preserve_thinking
    ?thinking_budget
    ()
;;

let kimi_config ?enable_thinking ?preserve_thinking ?thinking_budget model_id =
  PC.make
    ~kind:Kimi
    ~model_id
    ~base_url:"https://api.moonshot.ai/v1"
    ?enable_thinking
    ?preserve_thinking
    ?thinking_budget
    ()
;;

let ollama_config ?system_prompt ?enable_thinking model_id =
  PC.make
    ~kind:Ollama
    ~model_id
    ~base_url:"http://127.0.0.1:11434"
    ?system_prompt
    ?enable_thinking
    ()
;;

let ollama_cloud_config ?system_prompt ?enable_thinking model_id =
  PC.make
    ~kind:Ollama
    ~model_id
    ~base_url:"https://ollama.com"
    ~request_path:"/api/chat"
    ?system_prompt
    ?enable_thinking
    ()
;;

let anthropic_config ?enable_thinking ?thinking_budget ?output_schema model_id =
  PC.make
    ~kind:Anthropic
    ~model_id
    ~base_url:"https://api.anthropic.com"
    ~max_tokens:16_000
    ?enable_thinking
    ?thinking_budget
    ?output_schema
    ()
;;

let test_qwen_openai_compat_uses_chat_template_kwargs () =
  let config =
    openai_compat_config ~enable_thinking:false ~preserve_thinking:true "qwen3-32b"
  in
  let json = BOR.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
  let ctk = json |> member "chat_template_kwargs" in
  check bool "enable_thinking false" false (ctk |> member "enable_thinking" |> to_bool);
  check bool "preserve_thinking true" true (ctk |> member "preserve_thinking" |> to_bool);
  check_member_absent "thinking" json;
  check_member_absent "reasoning_effort" json;
  check_member_absent "think" json;
  check_member_absent "enable_thinking" json
;;

let test_qwen36_self_hosted_openai_compat_uses_chat_template_kwargs () =
  let config =
    openai_compat_config
      ~enable_thinking:false
      ~preserve_thinking:true
      "Qwen/Qwen3.6-35B-A3B"
  in
  let json = BOR.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
  let ctk = json |> member "chat_template_kwargs" in
  check bool "enable_thinking false" false (ctk |> member "enable_thinking" |> to_bool);
  check bool "preserve_thinking true" true (ctk |> member "preserve_thinking" |> to_bool);
  check_member_absent "thinking" json;
  check_member_absent "reasoning_effort" json;
  check_member_absent "enable_thinking" json
;;

let test_qwen36_reasoning_dialect_uses_chat_template_kwargs () =
  let config =
    openai_compat_config
      ~enable_thinking:false
      ~preserve_thinking:true
      "Qwen/Qwen3.6-35B-A3B"
  in
  let dialect = RD.for_provider_config config in
  check
    string
    "toggle wire"
    "chat_template_kwargs"
    (RD.toggle_wire_to_string dialect.toggle_wire);
  check string "visibility" "visible_channel" (RD.visibility_to_string dialect.visibility);
  check
    string
    "replay policy"
    "preserve_always"
    (RD.replay_policy_to_string dialect.replay_policy);
  check
    bool
    "no tool-call replay requirement"
    false
    (RD.requires_reasoning_replay_on_tool_call dialect);
  check
    (list string)
    "no ignored sampling params"
    []
    (RD.sampling_params_ignored_when_thinking dialect)
;;

let test_qwen36_reasoning_dialect_without_preserve_drops_reasoning () =
  let config =
    openai_compat_config
      ~enable_thinking:true
      ~preserve_thinking:false
      "Qwen/Qwen3.6-35B-A3B"
  in
  let dialect = RD.for_provider_config config in
  check
    string
    "replay policy"
    "no_replay"
    (RD.replay_policy_to_string dialect.replay_policy)
;;

let test_qwen36_dashscope_uses_top_level_enable_thinking () =
  let config =
    PC.make
      ~kind:DashScope
      ~model_id:"Qwen3.6-35B-A3B"
      ~base_url:"https://dashscope.aliyuncs.com/compatible-mode/v1"
      ~enable_thinking:true
      ~preserve_thinking:true
      ~thinking_budget:4096
      ()
  in
  let json = BOR.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
  check bool "enable_thinking true" true (json |> member "enable_thinking" |> to_bool);
  check bool "preserve_thinking true" true (json |> member "preserve_thinking" |> to_bool);
  check int "thinking budget" 4096 (json |> member "thinking_budget" |> to_int);
  check_member_absent "chat_template_kwargs" json;
  check_member_absent "thinking" json;
  check_member_absent "reasoning_effort" json
;;

let test_qwen36_dashscope_dialect_reports_enable_thinking () =
  (* The reported dialect metadata must match what build_request actually emits
     for a DashScope Qwen config (top-level enable_thinking, asserted above),
     not the model catalog's chat_template_kwargs. *)
  let config =
    PC.make
      ~kind:DashScope
      ~model_id:"Qwen3.6-35B-A3B"
      ~base_url:"https://dashscope.aliyuncs.com/compatible-mode/v1"
      ~enable_thinking:true
      ~preserve_thinking:true
      ()
  in
  let dialect = RD.for_provider_config config in
  check
    string
    "toggle wire"
    "enable_thinking"
    (RD.toggle_wire_to_string dialect.toggle_wire)
;;

let test_openai_reasoning_dialect_uses_reasoning_effort () =
  let dialect =
    RD.of_capabilities Llm_provider.Capabilities.openai_compat_chat_extended_capabilities
  in
  check
    string
    "toggle wire"
    "reasoning_effort"
    (RD.toggle_wire_to_string dialect.toggle_wire);
  check
    string
    "visibility"
    "side_channel:reasoning"
    (RD.visibility_to_string dialect.visibility);
  check
    string
    "replay policy"
    "no_replay"
    (RD.replay_policy_to_string dialect.replay_policy);
  check
    (option string)
    "preserve minimal"
    (Some "minimal")
    (RD.normalize_effort dialect "minimal");
  check (option string) "preserve high" (Some "high") (RD.normalize_effort dialect "high");
  check
    (option string)
    "preserve xhigh"
    (Some "xhigh")
    (RD.normalize_effort dialect "xhigh");
  check
    (option string)
    "typed preserve xhigh"
    (Some "xhigh")
    (RD.normalize_effort_value dialect RE.XHigh)
;;

let test_reasoning_visibility_override_visible_text () =
  let caps =
    { CAP.openai_compat_chat_extended_capabilities with
      reasoning_visibility_override = CAP.Force_visible_text
    }
  in
  let dialect = RD.of_capabilities caps in
  check string "visibility" "visible_text" (RD.visibility_to_string dialect.visibility)
;;

let test_openai_reasoning_request_uses_reasoning_effort () =
  with_manifest
    {|{"schema_version":1,"models":[{"id_prefix":"openai-reasoning-test-9fx","base":"openai_chat_extended","thinking_control_format":"reasoning_effort"}]}|}
    (fun () ->
       let config =
         PC.make
           ~kind:OpenAI_compat
           ~model_id:"openai-reasoning-test-9fx"
           ~base_url:"https://api.openai.com/v1"
           ~enable_thinking:true
           ~thinking_budget:4096
           ()
       in
       let json =
         BOR.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body
       in
       check
         string
         "reasoning_effort"
         "medium"
         (json |> member "reasoning_effort" |> to_string);
       check_member_absent "thinking" json;
       check_member_absent "enable_thinking" json;
       check_member_absent "chat_template_kwargs" json)
;;

let test_deepseek_openai_compat_uses_thinking_object () =
  let config =
    PC.make
      ~kind:OpenAI_compat
      ~model_id:"deepseek-v4-flash"
      ~base_url:"https://api.deepseek.com"
      ~enable_thinking:false
      ~thinking_budget:4096
      ()
  in
  let json = BOR.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
  check
    string
    "thinking type"
    "disabled"
    (json |> member "thinking" |> member "type" |> to_string);
  check_member_absent "reasoning_effort" json;
  check_member_absent "chat_template_kwargs" json;
  check_member_absent "think" json
;;

let test_deepseek_reasoning_dialect_semantics () =
  let config =
    PC.make
      ~kind:OpenAI_compat
      ~model_id:"deepseek-v4-pro"
      ~base_url:"https://api.deepseek.com"
      ~enable_thinking:true
      ()
  in
  let dialect = RD.for_provider_config config in
  check
    string
    "toggle wire"
    "thinking_object"
    (RD.toggle_wire_to_string dialect.toggle_wire);
  check
    string
    "visibility"
    "side_channel:reasoning_content"
    (RD.visibility_to_string dialect.visibility);
  check
    string
    "replay policy"
    "drop_without_tool_preserve_with_tool"
    (RD.replay_policy_to_string dialect.replay_policy);
  check
    bool
    "plain assistant reasoning may be dropped"
    false
    (RD.should_replay_reasoning dialect ~assistant_had_tool_call:false);
  check
    bool
    "tool-call assistant reasoning must replay"
    true
    (RD.should_replay_reasoning dialect ~assistant_had_tool_call:true);
  check
    bool
    "requires tool-call replay"
    true
    (RD.requires_reasoning_replay_on_tool_call dialect);
  check (option string) "low maps high" (Some "high") (RD.normalize_effort dialect "low");
  check
    (option string)
    "medium maps high"
    (Some "high")
    (RD.normalize_effort dialect "medium");
  check
    (option string)
    "xhigh maps max"
    (Some "max")
    (RD.normalize_effort dialect "xhigh");
  check
    (option string)
    "typed low maps high"
    (Some "high")
    (RD.normalize_effort_value dialect RE.Low);
  check
    (option string)
    "typed minimal omitted"
    None
    (RD.normalize_effort_value dialect RE.Minimal);
  check
    (list string)
    "ignored sampling params"
    [ "temperature"; "top_p"; "presence_penalty"; "frequency_penalty" ]
    (RD.sampling_params_ignored_when_thinking dialect)
;;

let test_deepseek_sampling_suppressed_in_thinking_mode () =
  let config =
    PC.make
      ~kind:OpenAI_compat
      ~model_id:"deepseek-v4-flash"
      ~base_url:"https://api.deepseek.com"
      ~temperature:0.7
      ~top_p:0.9
      ()
  in
  let json = BOR.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
  check_member_absent "temperature" json;
  check_member_absent "top_p" json
;;

let test_deepseek_disabled_thinking_keeps_sampling () =
  let config =
    PC.make
      ~kind:OpenAI_compat
      ~model_id:"deepseek-v4-flash"
      ~base_url:"https://api.deepseek.com"
      ~enable_thinking:false
      ~temperature:0.7
      ~top_p:0.9
      ()
  in
  let json = BOR.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
  check (float 0.001) "temperature" 0.7 (json |> member "temperature" |> to_float);
  check (float 0.001) "top_p" 0.9 (json |> member "top_p" |> to_float)
;;

let assistant_with_reasoning ?(tool = false) () =
  let content =
    if tool
    then
      [ Thinking { signature = None; content = "use calculator" }
      ; ToolUse { id = "call_1"; name = "calc"; input = `Assoc [ "expr", `String "2+2" ] }
      ]
    else
      [ Thinking { signature = None; content = "plain thought" }
      ; Text "answer"
      ]
  in
  { role = Assistant; content; name = None; tool_call_id = None; metadata = [] }
;;

let test_deepseek_replays_reasoning_only_for_tool_call_turns () =
  let config =
    PC.make
      ~kind:OpenAI_compat
      ~model_id:"deepseek-v4-flash"
      ~base_url:"https://api.deepseek.com"
      ()
  in
  let plain =
    BOR.build_request ~config ~messages:[ assistant_with_reasoning () ] () |> json_of_body
  in
  let tool =
    BOR.build_request ~config ~messages:[ assistant_with_reasoning ~tool:true () ] ()
    |> json_of_body
  in
  let plain_assistant = plain |> member "messages" |> index 0 in
  let tool_assistant = tool |> member "messages" |> index 0 in
  check_member_absent "reasoning_content" plain_assistant;
  check
    string
    "tool reasoning_content"
    "use calculator"
    (tool_assistant |> member "reasoning_content" |> to_string)
;;

let test_qwen_preserve_replays_reasoning_content () =
  let config =
    openai_compat_config
      ~enable_thinking:true
      ~preserve_thinking:true
      "Qwen/Qwen3.6-35B-A3B"
  in
  let json =
    BOR.build_request ~config ~messages:[ assistant_with_reasoning () ] () |> json_of_body
  in
  let assistant = json |> member "messages" |> index 0 in
  check
    string
    "reasoning_content"
    "plain thought"
    (assistant |> member "reasoning_content" |> to_string)
;;

let keep_all_axis_manifest =
  {|{"schema_version":1,"models":[{"id_prefix":"keep-all-axis-test","base":"openai_chat","supports_reasoning":true,"supports_extended_thinking":true,"thinking_control_format":"thinking_object_only","preserve_thinking_control_format":"thinking_object_keep_all"}]}|}
;;

let test_thinking_object_keep_all_axis_uses_keep_all () =
  with_manifest keep_all_axis_manifest (fun () ->
    let config = openai_compat_config ~preserve_thinking:true "keep-all-axis-test" in
    let json = BOR.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
    let thinking = json |> member "thinking" in
    check string "thinking type" "enabled" (thinking |> member "type" |> to_string);
    check string "thinking keep" "all" (thinking |> member "keep" |> to_string);
    check_member_absent "chat_template_kwargs" json;
    check_member_absent "preserve_thinking" json)
;;

let test_thinking_object_keep_all_axis_replays_reasoning () =
  with_manifest keep_all_axis_manifest (fun () ->
    let config = openai_compat_config ~preserve_thinking:true "keep-all-axis-test" in
    let dialect = RD.for_provider_config config in
    check
      string
      "toggle wire"
      "thinking_object_only"
      (RD.toggle_wire_to_string dialect.toggle_wire);
    check
      string
      "replay policy"
      "preserve_always"
      (RD.replay_policy_to_string dialect.replay_policy);
    check
      bool
      "no tool-call replay requirement"
      false
      (RD.requires_reasoning_replay_on_tool_call dialect);
    let json =
      BOR.build_request ~config ~messages:[ assistant_with_reasoning () ] ()
      |> json_of_body
    in
    let assistant = json |> member "messages" |> index 0 in
    check
      string
      "reasoning_content"
      "plain thought"
      (assistant |> member "reasoning_content" |> to_string))
;;

let test_kimi_latest_always_preserved_omits_thinking_param () =
  let config = kimi_config "kimi-k2.7-code" in
  let dialect = RD.for_provider_config config in
  check string "toggle wire" "no_toggle" (RD.toggle_wire_to_string dialect.toggle_wire);
  check
    string
    "replay policy"
    "preserve_always"
    (RD.replay_policy_to_string dialect.replay_policy);
  let user_json =
    BOR.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body
  in
  check_member_absent "thinking" user_json;
  check_member_absent "chat_template_kwargs" user_json;
  check_member_absent "preserve_thinking" user_json;
  let reasoning_json =
    BOR.build_request ~config ~messages:[ assistant_with_reasoning () ] () |> json_of_body
  in
  let assistant = reasoning_json |> member "messages" |> index 0 in
  check
    string
    "reasoning_content"
    "plain thought"
    (assistant |> member "reasoning_content" |> to_string)
;;

let test_ollama_qwen_uses_native_think_bool () =
  let config = ollama_config ~enable_thinking:true "qwen3:32b" in
  let json = BOL.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
  check bool "think true" true (json |> member "think" |> to_bool);
  check_member_absent "chat_template_kwargs" json;
  check_member_absent "thinking" json;
  check_member_absent "reasoning_effort" json
;;

let test_ollama_cloud_glm_uses_native_think_not_zai_thinking () =
  let config = ollama_cloud_config ~enable_thinking:true "glm-5.2" in
  let json = BOL.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
  check string "model" "glm-5.2" (json |> member "model" |> to_string);
  check bool "think true" true (json |> member "think" |> to_bool);
  check_member_absent "thinking" json;
  check_member_absent "reasoning_content" json;
  check_member_absent "reasoning_effort" json;
  check_member_absent "tool_stream" json;
  let dialect = RD.for_provider_config config in
  check string "toggle wire" "ollama_think" (RD.toggle_wire_to_string dialect.toggle_wire);
  check string "visibility" "visible_text" (RD.visibility_to_string dialect.visibility)
;;

let test_ollama_gemma4_enabled_uses_chat_template_token () =
  let config =
    ollama_config
      ~system_prompt:"You are a helpful assistant."
      ~enable_thinking:true
      "hf.co/unsloth/gemma-4-26B-A4B-it-qat-GGUF:UD-Q4_K_XL"
  in
  let json =
    BOL.build_request ~config ~messages:[ user_msg "solve 19*21" ] () |> json_of_body
  in
  check_member_absent "think" json;
  let first_message = json |> member "messages" |> index 0 in
  check string "system role" "system" (first_message |> member "role" |> to_string);
  check
    bool
    "system prompt starts with think token"
    true
    (String.starts_with
       ~prefix:"<|think|>\n"
       (first_message |> member "content" |> to_string))
;;

let test_ollama_gemma4_disabled_uses_native_think_false () =
  let config =
    ollama_config
      ~system_prompt:"You are a helpful assistant."
      ~enable_thinking:false
      "hf.co/unsloth/gemma-4-26B-A4B-it-qat-GGUF:UD-Q4_K_XL"
  in
  let json = BOL.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
  check bool "think false" false (json |> member "think" |> to_bool);
  let first_message = json |> member "messages" |> index 0 in
  check
    string
    "system prompt unchanged"
    "You are a helpful assistant."
    (first_message |> member "content" |> to_string)
;;

let test_gemma4_reasoning_dialect_uses_template_parser () =
  let config =
    ollama_config
      ~enable_thinking:true
      "hf.co/unsloth/gemma-4-26B-A4B-it-qat-GGUF:UD-Q4_K_XL"
  in
  let dialect = RD.for_provider_config config in
  check
    string
    "toggle wire"
    "chat_template_token"
    (RD.toggle_wire_to_string dialect.toggle_wire);
  check string "visibility" "visible_channel" (RD.visibility_to_string dialect.visibility);
  check
    string
    "replay policy"
    "no_replay"
    (RD.replay_policy_to_string dialect.replay_policy);
  check
    bool
    "no tool-call replay requirement"
    false
    (RD.requires_reasoning_replay_on_tool_call dialect);
  check
    (list string)
    "no ignored sampling params"
    []
    (RD.sampling_params_ignored_when_thinking dialect)
;;

let test_anthropic_reasoning_dialect_preserves_thinking () =
  let config =
    PC.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-6"
      ~base_url:"https://api.anthropic.com"
      ()
  in
  let dialect = RD.for_provider_config config in
  check
    string
    "toggle wire"
    "anthropic_thinking"
    (RD.toggle_wire_to_string dialect.toggle_wire);
  check
    string
    "replay policy"
    "preserve_always"
    (RD.replay_policy_to_string dialect.replay_policy)
;;

let test_anthropic_manual_model_uses_budget_tokens () =
  let config =
    anthropic_config ~enable_thinking:true ~thinking_budget:4096 "claude-opus-4-5"
  in
  let json = BAN.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
  let thinking = json |> member "thinking" in
  check string "thinking type" "enabled" (thinking |> member "type" |> to_string);
  check int "budget tokens" 4096 (thinking |> member "budget_tokens" |> to_int);
  check_member_absent "output_config" json
;;

let test_anthropic_opus48_uses_adaptive_effort () =
  let config =
    anthropic_config
      ~enable_thinking:true
      ~thinking_budget:(RE.low_budget_max_tokens + 1)
      "claude-opus-4-8"
  in
  let json = BAN.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
  let thinking = json |> member "thinking" in
  check string "thinking type" "adaptive" (thinking |> member "type" |> to_string);
  check_member_absent "budget_tokens" thinking;
  check
    string
    "effort"
    "medium"
    (json |> member "output_config" |> member "effort" |> to_string)
;;

let test_anthropic_agent_llm_alias_uses_adaptive_effort () =
  let config =
    anthropic_config
      ~enable_thinking:true
      ~thinking_budget:(RE.low_budget_max_tokens + 1)
      "claude-sonnet-4-6-20250514"
  in
  let json = BAN.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
  let thinking = json |> member "thinking" in
  check string "thinking type" "adaptive" (thinking |> member "type" |> to_string);
  check_member_absent "budget_tokens" thinking;
  check
    string
    "effort"
    "medium"
    (json |> member "output_config" |> member "effort" |> to_string)
;;

let test_anthropic_sonnet46_defaults_to_adaptive () =
  let config = anthropic_config ~enable_thinking:true "claude-sonnet-4-6" in
  let json = BAN.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
  let thinking = json |> member "thinking" in
  check string "thinking type" "adaptive" (thinking |> member "type" |> to_string);
  check_member_absent "budget_tokens" thinking;
  check_member_absent "output_config" json
;;

let test_anthropic_output_config_merges_format_and_effort () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "string" ] ]
      ]
  in
  let config =
    anthropic_config
      ~enable_thinking:true
      ~thinking_budget:(RE.high_budget_max_tokens + 1)
      ~output_schema:schema
      "claude-opus-4-8"
  in
  let json = BAN.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
  let output_config = json |> member "output_config" in
  check string "effort" "max" (output_config |> member "effort" |> to_string);
  check
    string
    "format type"
    "json_schema"
    (output_config |> member "format" |> member "type" |> to_string)
;;

let test_gemini_reasoning_dialect_uses_thinking_config () =
  let config =
    PC.make
      ~kind:Gemini
      ~model_id:"gemini-2.5-flash"
      ~base_url:"https://generativelanguage.googleapis.com/v1beta"
      ()
  in
  let dialect = RD.for_provider_config config in
  check
    string
    "toggle wire"
    "gemini_thinking_config"
    (RD.toggle_wire_to_string dialect.toggle_wire);
  check
    string
    "replay policy"
    "drop_without_tool_preserve_with_tool"
    (RD.replay_policy_to_string dialect.replay_policy)
;;

let () =
  run
    "thinking_control_dialects"
    [ ( "openai_compat"
      , [ test_case
            "qwen uses chat_template_kwargs"
            `Quick
            test_qwen_openai_compat_uses_chat_template_kwargs
        ; test_case
            "qwen3.6 self-hosted uses chat_template_kwargs"
            `Quick
            test_qwen36_self_hosted_openai_compat_uses_chat_template_kwargs
        ; test_case
            "qwen3.6 reasoning dialect uses chat_template_kwargs"
            `Quick
            test_qwen36_reasoning_dialect_uses_chat_template_kwargs
        ; test_case
            "qwen3.6 reasoning dialect without preserve drops reasoning"
            `Quick
            test_qwen36_reasoning_dialect_without_preserve_drops_reasoning
        ; test_case
            "qwen3.6 dashscope uses top-level enable_thinking"
            `Quick
            test_qwen36_dashscope_uses_top_level_enable_thinking
        ; test_case
            "qwen3.6 dashscope dialect reports enable_thinking"
            `Quick
            test_qwen36_dashscope_dialect_reports_enable_thinking
        ; test_case
            "openai reasoning dialect uses reasoning_effort"
            `Quick
            test_openai_reasoning_dialect_uses_reasoning_effort
        ; test_case
            "reasoning visibility override visible text"
            `Quick
            test_reasoning_visibility_override_visible_text
        ; test_case
            "openai reasoning request uses reasoning_effort"
            `Quick
            test_openai_reasoning_request_uses_reasoning_effort
        ; test_case
            "deepseek uses thinking object"
            `Quick
            test_deepseek_openai_compat_uses_thinking_object
        ; test_case
            "deepseek reasoning dialect semantics"
            `Quick
            test_deepseek_reasoning_dialect_semantics
        ; test_case
            "deepseek suppresses ignored sampling in thinking mode"
            `Quick
            test_deepseek_sampling_suppressed_in_thinking_mode
        ; test_case
            "deepseek disabled thinking keeps sampling"
            `Quick
            test_deepseek_disabled_thinking_keeps_sampling
        ; test_case
            "deepseek replays reasoning only for tool call turns"
            `Quick
            test_deepseek_replays_reasoning_only_for_tool_call_turns
        ; test_case
            "qwen preserve replays reasoning_content"
            `Quick
            test_qwen_preserve_replays_reasoning_content
        ; test_case
            "thinking_object_keep_all axis uses thinking keep all"
            `Quick
            test_thinking_object_keep_all_axis_uses_keep_all
        ; test_case
            "thinking_object_keep_all axis replays reasoning"
            `Quick
            test_thinking_object_keep_all_axis_replays_reasoning
        ; test_case
            "kimi latest always-preserved omits thinking param"
            `Quick
            test_kimi_latest_always_preserved_omits_thinking_param
        ] )
    ; ( "ollama"
      , [ test_case
            "qwen uses native think bool"
            `Quick
            test_ollama_qwen_uses_native_think_bool
        ; test_case
            "cloud GLM uses native think not ZAI thinking"
            `Quick
            test_ollama_cloud_glm_uses_native_think_not_zai_thinking
        ; test_case
            "gemma4 enabled uses chat template token"
            `Quick
            test_ollama_gemma4_enabled_uses_chat_template_token
        ; test_case
            "gemma4 disabled uses native think false"
            `Quick
            test_ollama_gemma4_disabled_uses_native_think_false
        ; test_case
            "gemma4 reasoning dialect uses template parser"
            `Quick
            test_gemma4_reasoning_dialect_uses_template_parser
        ] )
    ; ( "native"
      , [ test_case
            "anthropic reasoning dialect preserves thinking"
            `Quick
            test_anthropic_reasoning_dialect_preserves_thinking
        ; test_case
            "anthropic manual model uses budget_tokens"
            `Quick
            test_anthropic_manual_model_uses_budget_tokens
        ; test_case
            "anthropic opus 4.8 uses adaptive effort"
            `Quick
            test_anthropic_opus48_uses_adaptive_effort
        ; test_case
            "anthropic claude alias uses adaptive effort"
            `Quick
            test_anthropic_agent_llm_alias_uses_adaptive_effort
        ; test_case
            "anthropic sonnet 4.6 defaults to adaptive"
            `Quick
            test_anthropic_sonnet46_defaults_to_adaptive
        ; test_case
            "anthropic output_config merges format and effort"
            `Quick
            test_anthropic_output_config_merges_format_and_effort
        ; test_case
            "gemini reasoning dialect uses thinking config"
            `Quick
            test_gemini_reasoning_dialect_uses_thinking_config
        ] )
    ]
;;
