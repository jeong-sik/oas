(** Thinking-control dialect regression matrix.

    The same user-facing [enable_thinking] knob maps to different provider wire
    formats depending on the model family and serving backend. Keep these
    fixtures together so catalog changes cannot silently move a model family to
    the wrong dialect. *)

module PC = Llm_provider.Provider_config
module BOR = Llm_provider.Backend_openai_request
module BOL = Llm_provider.Backend_ollama
open Alcotest
open Llm_provider.Types
open Yojson.Safe.Util

let json_of_body body = Yojson.Safe.from_string body
let member_is_absent name json = json |> member name = `Null

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

let ollama_config ?system_prompt ?enable_thinking model_id =
  PC.make
    ~kind:Ollama
    ~model_id
    ~base_url:"http://127.0.0.1:11434"
    ?system_prompt
    ?enable_thinking
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

let test_ollama_qwen_uses_native_think_bool () =
  let config = ollama_config ~enable_thinking:true "qwen3:32b" in
  let json = BOL.build_request ~config ~messages:[ user_msg "hi" ] () |> json_of_body in
  check bool "think true" true (json |> member "think" |> to_bool);
  check_member_absent "chat_template_kwargs" json;
  check_member_absent "thinking" json;
  check_member_absent "reasoning_effort" json
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
            "qwen3.6 dashscope uses top-level enable_thinking"
            `Quick
            test_qwen36_dashscope_uses_top_level_enable_thinking
        ; test_case
            "deepseek uses thinking object"
            `Quick
            test_deepseek_openai_compat_uses_thinking_object
        ] )
    ; ( "ollama"
      , [ test_case
            "qwen uses native think bool"
            `Quick
            test_ollama_qwen_uses_native_think_bool
        ; test_case
            "gemma4 enabled uses chat template token"
            `Quick
            test_ollama_gemma4_enabled_uses_chat_template_token
        ; test_case
            "gemma4 disabled uses native think false"
            `Quick
            test_ollama_gemma4_disabled_uses_native_think_false
        ] )
    ]
;;
