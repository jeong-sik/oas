module Plan = Exact_output_plan
module Exec = Exact_output_execution
module Caps = Capabilities
module PC = Provider_config

type schema_fingerprint = Schema_fingerprint of string
type domain_schema = Domain_schema of Yojson.Safe.t

type minimum_guarantee =
  | Json_syntax
  | Provider_schema

type actual_assurance =
  | Json_syntax_only
  | Provider_schema_requested

type selected_target =
  { config : PC.t
  ; capabilities : Caps.capabilities
  }

type output_requirement =
  { schema : domain_schema
  ; source_schema_fingerprint : schema_fingerprint
  ; minimum_guarantee : minimum_guarantee
  }

type plan_provenance =
  { source_schema_fingerprint : schema_fingerprint
  ; effective_schema_fingerprint : schema_fingerprint option
  ; actual_assurance : actual_assurance
  }

type attempt_state =
  | Not_started_state
  | Before_dispatch_state
  | Dispatch_started_state
  | Response_received_state of int option
  | Terminal_state of int

type receipt =
  { state : attempt_state Atomic.t
  ; plan_fingerprint : string
  ; request_body_sha256 : string
  }

type ready_plan =
  { plan : Plan.t
  ; provenance : plan_provenance
  ; receipt : receipt
  }

type target_selection_error =
  | Unknown_target of string
  | Missing_target_model of { selector : string }
  | Missing_target_credential of
      { selector : string
      ; environment_variable : string
      }

type wire_admission_error =
  | Capability_snapshot_missing
  | Inconsistent_output_contract
  | Output_contract_unavailable
  | Cross_feature_not_allowed
  | Global_admission_not_allowed
  | Invalid_connect_timeout
  | Invalid_body_timeout
  | Framing_header_not_allowed
  | Target_request_rejected
  | Request_serialization_rejected

type admission_error =
  | Provider_schema_unavailable
  | Json_syntax_unavailable
  | Wire_admission_rejected of wire_admission_error

type effect_phase =
  | Not_started
  | Before_dispatch
  | Dispatch_started
  | Response_received
  | Terminal

type raw_response =
  { body : string
  ; body_sha256 : string
  }

type execution_error_cause =
  | Attempt_already_started
  | Clock_required_for_timeout
  | Frozen_request_mismatch
  | Completion_failed
  | Incomplete_output
  | Missing_output
  | Ambiguous_output of int
  | Unexpected_output_content
  | Invalid_json_output
  | Internal_non_json_output

type execution_error =
  { receipt : receipt
  ; cause : execution_error_cause
  ; raw_response : raw_response option
  }

type success =
  { receipt : receipt
  ; output : Yojson.Safe.t
  ; provenance : plan_provenance
  ; raw_response : raw_response
  }

let ( let* ) = Result.bind

let rec canonical_json = function
  | `Assoc fields ->
    `Assoc
      (fields
       |> List.map (fun (name, value) -> name, canonical_json value)
       |> List.sort (fun (left, _) (right, _) -> String.compare left right))
  | `List values -> `List (List.map canonical_json values)
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as scalar -> scalar
;;

let fingerprint_schema schema =
  canonical_json schema
  |> Yojson.Safe.to_string
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
  |> fun value -> Schema_fingerprint value
;;

let schema_fingerprint_to_string (Schema_fingerprint value) = value

let make_output_requirement ~schema ~minimum_guarantee =
  { schema = Domain_schema schema
  ; source_schema_fingerprint = fingerprint_schema schema
  ; minimum_guarantee
  }
;;

let credential_for_entry ~selector (entry : Provider_catalog.entry) =
  match entry.auth with
  | Provider_catalog.No_auth -> Ok ""
  | Provider_catalog.Api_key_env environment_variable
  | Provider_catalog.Setup_token_env environment_variable ->
    (match Cli_common_env.get environment_variable with
     | Some value -> Ok value
     | None -> Error (Missing_target_credential { selector; environment_variable }))
;;

let trimmed_nonempty value =
  let value = String.trim value in
  if String.equal value "" then None else Some value
;;

let select_target ~selector ?model () =
  match Provider_catalog.global () with
  | None -> Error (Unknown_target selector)
  | Some catalog ->
    (match Provider_catalog.lookup catalog selector with
     | None -> Error (Unknown_target selector)
     | Some entry ->
       let model_id =
         match Option.bind model trimmed_nonempty with
         | Some model_id -> Some model_id
         | None -> Option.bind entry.default_model trimmed_nonempty
       in
       (match model_id with
        | None -> Error (Missing_target_model { selector })
        | Some model_id ->
          let* credential = credential_for_entry ~selector entry in
          let capabilities =
            match
              Caps.for_provider_model_id
                ~allow_bare_fallback:false
                ~provider_label:entry.id
                ~model_id
            with
            | Some capabilities -> capabilities
            | None -> entry.capabilities
          in
          let request_path = Option.bind (Some entry.request_path) trimmed_nonempty in
          let max_context = entry.max_context in
          let config =
            PC.make
              ~kind:entry.kind
              ~provider_id:entry.id
              ~model_id
              ~base_url:entry.base_url
              ?request_path
              ?max_context
              ()
          in
          Ok
            { config =
                { config with
                  api_key = Secret.of_string credential
                ; model_capabilities_override = Some capabilities
                }
            ; capabilities
            }))
;;

let schema_for_wire target (Domain_schema domain_schema) =
  match Provider_http_codec.(json_schema_wire (of_config target.config)) with
  | Raw_schema -> domain_schema
  | Openai_named_schema ->
    `Assoc
      [ "name", `String (Provider_config.structured_output_name_of_schema domain_schema)
      ; "schema", domain_schema
      ; "strict", `Bool true
      ]
;;

let response_format target requirement =
  match
    Caps.structured_output_support target.capabilities, requirement.minimum_guarantee
  with
  | Caps.Native_json_schema, (Json_syntax | Provider_schema) ->
    let wire_schema = schema_for_wire target requirement.schema in
    Ok
      ( Types.JsonSchema wire_schema
      , Provider_schema_requested
      , Some (fingerprint_schema wire_schema) )
  | Caps.Json_object_only, Json_syntax -> Ok (Types.JsonMode, Json_syntax_only, None)
  | Caps.Json_object_only, Provider_schema -> Error Provider_schema_unavailable
  | Caps.No_structured_output, Provider_schema -> Error Provider_schema_unavailable
  | Caps.No_structured_output, Json_syntax -> Error Json_syntax_unavailable
;;

let exact_config target response_format =
  let output_schema = PC.output_schema_of_response_format response_format in
  { target.config with
    temperature = None
  ; top_p = None
  ; top_k = None
  ; min_p = None
  ; system_prompt = None
  ; enable_thinking = None
  ; preserve_thinking = None
  ; thinking_budget = None
  ; reasoning_effort = None
  ; clear_thinking = None
  ; tool_stream = false
  ; tool_choice = None
  ; disable_parallel_tool_use = false
  ; response_format
  ; output_schema
  ; cache_system_prompt = false
  ; keep_alive = None
  ; internal_model_rotation_count = None
  ; previous_response_id = None
  ; max_concurrent_requests = None
  ; model_capabilities_override = Some target.capabilities
  }
;;

let wire_admission_error = function
  | Plan.Explicit_capability_snapshot_required -> Capability_snapshot_missing
  | Plan.Contradictory_output_state -> Inconsistent_output_contract
  | Plan.Unsupported_output_contract _ -> Output_contract_unavailable
  | Plan.Unsupported_exact_cross_feature -> Cross_feature_not_allowed
  | Plan.Global_admission_not_allowed -> Global_admission_not_allowed
  | Plan.Invalid_connect_timeout _ -> Invalid_connect_timeout
  | Plan.Invalid_body_timeout _ -> Invalid_body_timeout
  | Plan.Caller_supplied_framing_header_not_allowed _ -> Framing_header_not_allowed
  | Plan.Provider_request_rejected _ -> Target_request_rejected
  | Plan.Request_serialization_rejected _ -> Request_serialization_rejected
;;

let admit ~target ~messages requirement =
  let* response_format, actual_assurance, effective_schema_fingerprint =
    response_format target requirement
  in
  Plan.admit (Plan.Unmeasured { config = exact_config target response_format; messages })
  |> Result.map_error (fun error -> Wire_admission_rejected (wire_admission_error error))
  |> Result.map (fun plan ->
    let plan_fingerprint = Plan.fingerprint plan |> Plan.fingerprint_to_string in
    { plan
    ; provenance =
        { source_schema_fingerprint = requirement.source_schema_fingerprint
        ; effective_schema_fingerprint
        ; actual_assurance
        }
    ; receipt =
        { state = Atomic.make Not_started_state
        ; plan_fingerprint
        ; request_body_sha256 = Plan.request_body_sha256 plan
        }
    })
;;

let plan_provenance (ready : ready_plan) = ready.provenance
let plan_fingerprint (ready : ready_plan) = ready.receipt.plan_fingerprint
let attempt_receipt (ready : ready_plan) = ready.receipt

let receipt_phase receipt =
  match Atomic.get receipt.state with
  | Not_started_state -> Not_started
  | Before_dispatch_state -> Before_dispatch
  | Dispatch_started_state -> Dispatch_started
  | Response_received_state _ -> Response_received
  | Terminal_state _ -> Terminal
;;

let receipt_dispatch_count receipt =
  match Atomic.get receipt.state with
  | Not_started_state | Before_dispatch_state -> 0
  | Dispatch_started_state | Response_received_state _ | Terminal_state _ -> 1
;;

let receipt_http_status receipt =
  match Atomic.get receipt.state with
  | Response_received_state status -> status
  | Terminal_state status -> Some status
  | Not_started_state | Before_dispatch_state | Dispatch_started_state -> None
;;

let receipt_plan_fingerprint receipt = receipt.plan_fingerprint
let receipt_request_body_sha256 receipt = receipt.request_body_sha256

let state_rank = function
  | Not_started_state -> 0
  | Before_dispatch_state -> 1
  | Dispatch_started_state -> 2
  | Response_received_state _ -> 3
  | Terminal_state _ -> 4
;;

let rec advance receipt desired =
  let current = Atomic.get receipt.state in
  let adds_information =
    state_rank desired > state_rank current
    ||
    match current, desired with
    | Response_received_state None, Response_received_state (Some _) -> true
    | _ -> false
  in
  if adds_information
  then
    if not (Atomic.compare_and_set receipt.state current desired)
    then advance receipt desired
;;

let observe_phase receipt = function
  | Http_client_phase_observer.Dispatch_started -> advance receipt Dispatch_started_state
  | Http_client_phase_observer.Response_received status ->
    advance receipt (Response_received_state (Some status))
;;

let synchronize_receipt receipt complete_receipt =
  match Exec.receipt_phase complete_receipt with
  | Exec.Before_dispatch -> advance receipt Before_dispatch_state
  | Exec.Dispatch_started -> advance receipt Dispatch_started_state
  | Exec.Response_received ->
    advance receipt (Response_received_state (Exec.receipt_http_status complete_receipt))
  | Exec.Terminal ->
    (match Exec.receipt_http_status complete_receipt with
     | Some status -> advance receipt (Terminal_state status)
     | None -> invalid_arg "Exact_output: terminal receipt without HTTP status")
;;

let raw_response (evidence : Exec.raw_response_evidence) =
  { body = evidence.raw_body; body_sha256 = evidence.raw_body_sha256 }
;;

let execution_error_cause = function
  | Exec.Clock_required_for_timeout -> Clock_required_for_timeout
  | Exec.Frozen_request_mismatch -> Frozen_request_mismatch
  | Exec.Provider_error _ -> Completion_failed
  | Exec.Output_normalization_failed (Exec.Incomplete_structured_response _) ->
    Incomplete_output
  | Exec.Output_normalization_failed Exec.Missing_structured_text -> Missing_output
  | Exec.Output_normalization_failed (Exec.Ambiguous_structured_text count) ->
    Ambiguous_output count
  | Exec.Output_normalization_failed Exec.Unexpected_structured_content ->
    Unexpected_output_content
  | Exec.Output_normalization_failed (Exec.Invalid_json _) -> Invalid_json_output
;;

let execute_once ~net ?clock (ready : ready_plan) =
  if
    not
      (Atomic.compare_and_set ready.receipt.state Not_started_state Before_dispatch_state)
  then
    Error
      { receipt = ready.receipt; cause = Attempt_already_started; raw_response = None }
  else (
    match
      Exec.execute_once_with_evidence
        ~net
        ?clock
        ~on_phase:(observe_phase ready.receipt)
        ready.plan
    with
    | Error
        ({ receipt; cause; raw_response = evidence } :
          Exec.execute_once_error_with_evidence) ->
      synchronize_receipt ready.receipt receipt;
      Error
        { receipt = ready.receipt
        ; cause = execution_error_cause cause
        ; raw_response = Option.map raw_response evidence
        }
    | Ok { outcome; raw_response = evidence } ->
      synchronize_receipt ready.receipt outcome.receipt;
      (match outcome.output with
       | Exec.Json_output { value; _ } ->
         Ok
           { receipt = ready.receipt
           ; output = value
           ; provenance = ready.provenance
           ; raw_response = raw_response evidence
           }
       | Exec.Text_output _ ->
         Error
           { receipt = ready.receipt
           ; cause = Internal_non_json_output
           ; raw_response = Some (raw_response evidence)
           }))
;;
