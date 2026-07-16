open Alcotest
open Llm_provider
open Input_token_count

let protocols =
  [ Anthropic_messages_count_tokens, "input_tokens"
  ; Openai_responses_input_tokens, "input_tokens"
  ; Gemini_count_tokens, "totalTokens"
  ]
;;

let body field value = Printf.sprintf {|{"%s":%s}|} field value

let expect_count ~protocol ~model_id expected response =
  match decode_response ~protocol ~model_id response with
  | Ok count ->
    check int "input tokens" expected count.input_tokens;
    check string "model id is exact" model_id count.model_id;
    check bool "protocol is exact" true (equal_protocol protocol count.protocol)
  | Error _ -> fail "expected a decoded input-token count"
;;

let expect_invalid ~protocol ~model_id response =
  match decode_response ~protocol ~model_id response with
  | Error (Invalid_response { protocol = actual; model_id = actual_model; detail }) ->
    check bool "protocol evidence" true (equal_protocol protocol actual);
    check string "model evidence" model_id actual_model;
    check bool "detail is explicit" true (String.length detail > 0)
  | Error (Unsupported _ | Transport _) -> fail "expected Invalid_response"
  | Ok _ -> fail "expected response rejection"
;;

let test_positive_counts () =
  List.iter
    (fun (protocol, field) ->
       expect_count ~protocol ~model_id:"model/exact" 42 (body field "42"))
    protocols
;;

let test_zero_is_valid () =
  List.iter
    (fun (protocol, field) ->
       expect_count ~protocol ~model_id:"zero-model" 0 (body field "0"))
    protocols
;;

let test_missing_field_is_invalid () =
  List.iter
    (fun (protocol, _) ->
       expect_invalid ~protocol ~model_id:"missing-model" {|{"other":1}|})
    protocols
;;

let test_non_integer_is_invalid () =
  List.iter
    (fun (protocol, field) ->
       expect_invalid ~protocol ~model_id:"non-integer-model" (body field {|1.5|}))
    protocols
;;

let test_negative_is_invalid () =
  List.iter
    (fun (protocol, field) ->
       expect_invalid ~protocol ~model_id:"negative-model" (body field "-1"))
    protocols
;;

let test_out_of_range_is_invalid () =
  List.iter
    (fun (protocol, field) ->
       expect_invalid
         ~protocol
         ~model_id:"out-of-range-model"
         (body field "999999999999999999999999999999999999"))
    protocols
;;

let test_malformed_and_non_object_are_invalid () =
  List.iter
    (fun (protocol, _) ->
       expect_invalid ~protocol ~model_id:"bad-json-model" "{";
       expect_invalid ~protocol ~model_id:"array-model" "[]")
    protocols
;;

let test_transport_error_is_preserved () =
  let transport =
    Http_client.TimeoutError
      { message = "provider deadline"; phase = Http_client.First_token }
  in
  match
    decode_transport_result
      ~protocol:Openai_responses_input_tokens
      ~model_id:"transport-model"
      (Error transport)
  with
  | Error (Transport (Http_client.TimeoutError { message; phase })) ->
    check string "message" "provider deadline" message;
    check bool "phase" true (phase = Http_client.First_token)
  | Error (Unsupported _ | Invalid_response _ | Transport _) ->
    fail "transport error shape changed"
  | Ok _ -> fail "transport error was silently accepted"
;;

let test_transport_success_is_decoded () =
  match
    decode_transport_result
      ~protocol:Gemini_count_tokens
      ~model_id:"transport-success-model"
      (Ok {|{"totalTokens":7}|})
  with
  | Ok count -> check int "input tokens" 7 count.input_tokens
  | Error _ -> fail "successful transport body was not decoded"
;;

let () =
  run
    "input-token-count"
    [ ( "response"
      , [ test_case "positive counts" `Quick test_positive_counts
        ; test_case "zero is valid" `Quick test_zero_is_valid
        ; test_case "missing field is invalid" `Quick test_missing_field_is_invalid
        ; test_case "non-integer is invalid" `Quick test_non_integer_is_invalid
        ; test_case "negative is invalid" `Quick test_negative_is_invalid
        ; test_case "out-of-range is invalid" `Quick test_out_of_range_is_invalid
        ; test_case
            "malformed and non-object are invalid"
            `Quick
            test_malformed_and_non_object_are_invalid
        ] )
    ; ( "transport"
      , [ test_case
            "typed HTTP error is preserved"
            `Quick
            test_transport_error_is_preserved
        ; test_case "successful body is decoded" `Quick test_transport_success_is_decoded
        ] )
    ]
;;
