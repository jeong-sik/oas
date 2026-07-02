(* Provider-agnostic multi-turn reasoning-replay harness (#2236).

   Goal under test (user acceptance criteria): regardless of which model
   provider is configured, interleaving / tool-use / CoT must behave the SAME
   way, and the model's reasoning must NEVER be re-injected as the assistant's
   answer (the parrot / infinite-recursion loop).

   This harness drives EVERY exported provider capability profile through the
   live request serializer ([Backend_openai_serialize.dialect_messages_of_message],
   the function both live request builders call) and asserts two invariants:

   INV1 (universal anti-parrot): for every provider, every assistant message
   shape, the serialized assistant [content] (answer) field never contains the
   reasoning text. This is the loop-closure guarantee: the model is never fed
   its own reasoning as a finalized answer, so it cannot re-reason over it.

   INV2 (uniform interleaving contract): the decision to replay reasoning is the
   SAME function of (replay_policy, had_tool_call) for every provider —
   [Reasoning_dialect.should_replay_reasoning]. A tool-call turn that replays
   carries the reasoning in the dedicated [reasoning_content] field (satisfying
   the DeepSeek / MiMo "400 if tool-call reasoning missing" contract); a plain
   answer turn under a drop policy carries no [reasoning_content]. Either way,
   reasoning stays out of [content] (INV1 still holds).

   The harness is deterministic: it constructs messages and inspects wire JSON;
   no live API or network. *)

open Llm_provider
open Types
module Serialize = Backend_openai_serialize
module RD = Reasoning_dialect

let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let member = Yojson.Safe.Util.member

let msg role content : message =
  { role; content; name = None; tool_call_id = None; metadata = [] }
;;

(* A reasoning marker that must never surface as answer content. *)
let reasoning_marker = "REASONING_MUST_NOT_BECOME_ANSWER"
let answer_text = "the visible answer"

(* Every exported provider capability profile plus representative catalog model
   entries for current frontier families. Provider profiles cover protocol base
   behavior; catalog entries cover model-specific thinking / replay overrides
   where the latest Kimi, MiniMax, GLM, DeepSeek, Qwen, MiMo, and DashScope
   contracts actually live. *)
let base_provider_profiles : (string * Capabilities.capabilities) list =
  [ "default", Capabilities.default_capabilities
  ; "anthropic", Capabilities.anthropic_capabilities
  ; "kimi", Capabilities.kimi_capabilities
  ; "openai_compat_chat", Capabilities.openai_compat_chat_capabilities
  ; "openai_compat_chat_extended", Capabilities.openai_compat_chat_extended_capabilities
  ; "gemini", Capabilities.gemini_capabilities
  ; "ollama", Capabilities.ollama_capabilities
  ; "ollama_cloud", Capabilities.ollama_cloud_capabilities
  ; "dashscope", Capabilities.dashscope_capabilities
  ; "glm", Capabilities.glm_capabilities
  ; "provider_l", Capabilities.provider_l_capabilities
  ]
;;

let catalog_model_profile_resolvers
  : (string * (unit -> Capabilities.capabilities option)) list
  =
  [ ("model:mimo-v2.5-pro", fun () -> Capabilities.for_model_id "mimo-v2.5-pro")
  ; ("model:deepseek-v4-flash", fun () -> Capabilities.for_model_id "deepseek-v4-flash")
  ; ("model:deepseek-v4-pro", fun () -> Capabilities.for_model_id "deepseek-v4-pro")
  ; ("model:minimax-m3", fun () -> Capabilities.for_model_id "minimax-m3")
  ; ("model:kimi-k2.7-code", fun () -> Capabilities.for_model_id "kimi-k2.7-code")
  ; ("model:glm-5.2", fun () -> Capabilities.for_model_id "glm-5.2")
  ; ( "model:qwen/qwen3.6-35b-a3b"
    , fun () -> Capabilities.for_model_id "qwen/qwen3.6-35b-a3b" )
  ; ( "model:dashscope-3.5-35b-a3b"
    , fun () -> Capabilities.for_model_id "dashscope-3.5-35b-a3b" )
  ; ( "provider:vllm-qwen3-mtp.vllm-qwen3-mtp.qwen36-35b-a3b-mtp"
    , fun () ->
        Capabilities.for_provider_model_id
          ~allow_bare_fallback:false
          ~provider_label:"vllm-qwen3-mtp"
          ~model_id:"vllm-qwen3-mtp.qwen36-35b-a3b-mtp" )
  ]
;;

let profile_cases () =
  Model_catalog_test_support.install_repo_model_catalog
    ~suite:"provider_agnostic_reasoning_replay";
  let catalog_profiles =
    List.map
      (fun (name, resolve) ->
         match resolve () with
         | Some caps -> name, caps
         | None -> Alcotest.failf "expected catalog capabilities for %s" name)
      catalog_model_profile_resolvers
  in
  base_provider_profiles @ catalog_profiles
;;

let serialized_assistant dialect msg =
  match Serialize.dialect_messages_of_message dialect msg with
  | [ j ] -> j
  | js -> Alcotest.failf "expected one assistant message, got %d" (List.length js)
;;

let content_string j =
  match member "content" j with
  | `String s -> s
  | `Null -> ""
  | other -> Yojson.Safe.to_string other
;;

let contains ~needle haystack =
  let nl = String.length needle in
  let hl = String.length haystack in
  if nl = 0
  then true
  else (
    let rec scan i =
      if i + nl > hl
      then false
      else if String.sub haystack i nl = needle
      then true
      else scan (i + 1)
    in
    scan 0)
;;

(* INV1: across every provider and every assistant shape that carries reasoning,
   the answer [content] field never contains the reasoning marker. *)
let test_reasoning_never_in_content_any_provider () =
  List.iter
    (fun (name, caps) ->
       let dialect = RD.of_capabilities caps in
       let shapes =
         [ "reasoning_only", [ Thinking { content = reasoning_marker; signature = None } ]
         ; ( "reasoning_then_answer"
           , [ Thinking { content = reasoning_marker; signature = None }
             ; Text answer_text
             ] )
         ; ( "reasoning_then_tool_call"
           , [ Thinking { content = reasoning_marker; signature = None }
             ; ToolUse
                 { id = "call-1"; name = "lookup"; input = `Assoc [ "q", `String "x" ] }
             ] )
         ]
       in
       List.iter
         (fun (shape, content) ->
            let j = serialized_assistant dialect (msg Assistant content) in
            let c = content_string j in
            check_bool
              (Printf.sprintf
                 "[%s/%s] reasoning marker must NOT appear in answer content"
                 name
                 shape)
              false
              (contains ~needle:reasoning_marker c))
         shapes)
    (profile_cases ())
;;

(* INV2: the replay decision is one shared function of (policy, had_tool_call),
   identical across providers. For each provider, a tool-call turn either carries
   the reasoning in [reasoning_content] (when the policy replays with a tool
   call) or omits it — but NEVER puts it in [content]. A plain reasoning+answer
   turn carries [reasoning_content] iff the policy replays without a tool call.
   We assert the wire matches [should_replay_reasoning] exactly. *)
let reasoning_content_string j =
  match member "reasoning_content" j with
  | `String s -> s
  | _ -> ""
;;

let test_replay_matches_should_replay_reasoning_any_provider () =
  List.iter
    (fun (name, caps) ->
       let dialect = RD.of_capabilities caps in
       (* tool-call turn *)
       let tool_msg =
         msg
           Assistant
           [ Thinking { content = reasoning_marker; signature = None }
           ; ToolUse { id = "c"; name = "f"; input = `Assoc [] }
           ]
       in
       let expect_tool =
         RD.should_replay_reasoning dialect ~assistant_had_tool_call:true
       in
       let j_tool = serialized_assistant dialect tool_msg in
       check_bool
         (Printf.sprintf
            "[%s] tool-call turn: reasoning_content present iff policy replays"
            name)
         expect_tool
         (reasoning_content_string j_tool = reasoning_marker);
       (* plain reasoning+answer turn (no tool call) *)
       let plain_msg =
         msg
           Assistant
           [ Thinking { content = reasoning_marker; signature = None }; Text answer_text ]
       in
       let expect_plain =
         RD.should_replay_reasoning dialect ~assistant_had_tool_call:false
       in
       let j_plain = serialized_assistant dialect plain_msg in
       check_bool
         (Printf.sprintf
            "[%s] plain answer turn: reasoning_content present iff policy replays"
            name)
         expect_plain
         (reasoning_content_string j_plain = reasoning_marker);
       (* INV1 reaffirmed on the plain turn: the answer text is the only content *)
       check_string
         (Printf.sprintf "[%s] plain answer content is the answer text only" name)
         answer_text
         (content_string j_plain))
    (profile_cases ())
;;

(* INV3 (recursion closure): simulate N replay rounds of a reasoning-only reply
   fed back into history. The serialized content must stay empty every round —
   the reasoning never accumulates into the answer channel, so there is no
   growing parrot. We drive the same dialect repeatedly to model a stuck turn. *)
let test_reasoning_only_replay_does_not_accumulate () =
  List.iter
    (fun (name, caps) ->
       let dialect = RD.of_capabilities caps in
       let reasoning_only =
         msg Assistant [ Thinking { content = reasoning_marker; signature = None } ]
       in
       for round = 1 to 5 do
         let j = serialized_assistant dialect reasoning_only in
         let c = content_string j in
         check_string
           (Printf.sprintf "[%s] round %d reasoning-only content stays empty" name round)
           ""
           c
       done)
    (profile_cases ())
;;

(* End-to-end loop closure: parse a real provider response carrying
   [reasoning_content] (the wire shape DeepSeek / MiMo / Ollama / GLM all emit),
   then serialize the parsed assistant message back through every dialect. This
   exercises the actual loop path (parse -> history -> serialize) rather than a
   hand-built Thinking block, so it catches a regression on EITHER side: a parse
   that re-introduces the reasoning->Text promotion, or a serialize that leaks
   reasoning into content. The parser is dialect-independent (it always yields a
   [Thinking] block from [reasoning_content]); only serialization is per-dialect,
   so one parse feeds every provider. *)
let reasoning_only_response_json =
  `Assoc
    [ "id", `String "resp-1"
    ; "model", `String "test"
    ; ( "choices"
      , `List
          [ `Assoc
              [ "index", `Int 0
              ; "finish_reason", `String "stop"
              ; ( "message"
                , `Assoc
                    [ "role", `String "assistant"
                    ; "content", `String ""
                    ; "reasoning_content", `String reasoning_marker
                    ] )
              ]
          ] )
    ]
;;

let test_parse_then_serialize_round_trip_any_provider () =
  let response =
    match
      Backend_openai_parse.parse_openai_response_result
        (Yojson.Safe.to_string reasoning_only_response_json)
    with
    | Ok r -> r
    | Error msg -> Alcotest.failf "unexpected parse error: %s" msg
  in
  (* Parse must keep reasoning typed as Thinking, never a Text answer block. *)
  let has_text =
    List.exists
      (function
        | Text _ -> true
        | _ -> false)
      response.content
  in
  let has_thinking =
    List.exists
      (function
        | Thinking { content; _ } -> String.trim content = reasoning_marker
        | _ -> false)
      response.content
  in
  check_bool "parse keeps reasoning typed as Thinking" true has_thinking;
  check_bool "parse never promotes reasoning to a Text answer block" false has_text;
  let assistant = msg Assistant response.content in
  List.iter
    (fun (name, caps) ->
       let dialect = RD.of_capabilities caps in
       let j = serialized_assistant dialect assistant in
       check_string
         (Printf.sprintf
            "[%s] parsed reasoning-only reply serializes to empty answer content"
            name)
         ""
         (content_string j);
       check_bool
         (Printf.sprintf "[%s] reasoning marker absent from answer content" name)
         false
         (contains ~needle:reasoning_marker (content_string j)))
    (profile_cases ())
;;

let () =
  Alcotest.run
    "provider_agnostic_reasoning_replay"
    [ ( "anti-parrot (#2236)"
      , [ Alcotest.test_case
            "reasoning never appears in answer content (all providers)"
            `Quick
            test_reasoning_never_in_content_any_provider
        ; Alcotest.test_case
            "wire replay matches should_replay_reasoning (all providers)"
            `Quick
            test_replay_matches_should_replay_reasoning_any_provider
        ; Alcotest.test_case
            "reasoning-only replay never accumulates into content (all providers)"
            `Quick
            test_reasoning_only_replay_does_not_accumulate
        ; Alcotest.test_case
            "parse->serialize round-trip keeps reasoning out of content (all providers)"
            `Quick
            test_parse_then_serialize_round_trip_any_provider
        ] )
    ]
;;
