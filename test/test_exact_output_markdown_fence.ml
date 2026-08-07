(** [strip_enclosing_markdown_fence] on the structured-output parse path.

    A model asked for JSON without [response_format] decides on its own
    whether to fence the object. Measured 2026-08-08 against
    glm-coding.glm-5-turbo on a 39,127-token prompt whose instructions end
    with "Respond with ONLY the JSON object, no markdown": two of five replies
    opened with a fence and three did not, so the same request failed at
    random on a body that parses once the fence is removed. The same prompt on
    kimi-for-coding fenced zero times out of five.

    The risk in a change like this is that it stops being fence removal and
    becomes a repair pass over malformed output. Half of these cases exist to
    pin what must pass through untouched. *)

module Exact_output = Agent_sdk.Exact_output

let strip = Exact_output.strip_enclosing_markdown_fence
let check name expected input = Alcotest.(check string) name expected (strip input)

(* The shape actually observed on the wire. *)
let test_fenced_object_is_unwrapped () =
  check
    "json tag"
    {|{"retained_memory_ids": ["id:1"]}|}
    "```json\n{\"retained_memory_ids\": [\"id:1\"]}\n```";
  check "no tag" {|{"a":1}|} "```\n{\"a\":1}\n```";
  check "surrounding whitespace" {|{"a":1}|} "\n  ```json\n{\"a\":1}\n```  \n"
;;

let test_unfenced_body_is_returned_verbatim () =
  check "bare object" {|{"a":1}|} {|{"a":1}|};
  (* Whitespace is part of the body when there is no fence: trimming here
     would make the function do something other than what it says. *)
  check "leading newline kept" "\n{\"a\":1}" "\n{\"a\":1}"
;;

(* A fence that does not enclose the whole body is prose that happens to
   contain one. Removing the opening line would corrupt it. *)
let test_partial_fence_is_left_alone () =
  check
    "no closing fence"
    "```json\n{\"a\":1}"
    "```json\n{\"a\":1}";
  check
    "text before the fence"
    "here you go:\n```json\n{\"a\":1}\n```"
    "here you go:\n```json\n{\"a\":1}\n```";
  check "opening fence only" "```" "```"
;;

(* The opening line carries the fence and at most a language tag. A line with
   anything else is not a fence this may touch. *)
let test_opening_line_must_be_only_a_tag () =
  check
    "prose on the fence line"
    "```json here is the object\n{\"a\":1}\n```"
    "```json here is the object\n{\"a\":1}\n```"
;;

(* Not a repair pass: a fenced body that is still not JSON comes out as the
   same non-JSON, and the caller reports it exactly as before. *)
let test_fence_removal_does_not_repair_content () =
  check "still invalid" "{not json" "```json\n{not json\n```"
;;

let () =
  Alcotest.run
    "exact output markdown fence"
    [ ( "unwraps"
      , [ Alcotest.test_case "fenced object is unwrapped" `Quick
            test_fenced_object_is_unwrapped
        ] )
    ; ( "leaves alone"
      , [ Alcotest.test_case "unfenced body is returned verbatim" `Quick
            test_unfenced_body_is_returned_verbatim
        ; Alcotest.test_case "partial fence is left alone" `Quick
            test_partial_fence_is_left_alone
        ; Alcotest.test_case "opening line must be only a tag" `Quick
            test_opening_line_must_be_only_a_tag
        ; Alcotest.test_case "fence removal does not repair content" `Quick
            test_fence_removal_does_not_repair_content
        ] )
    ]
;;
