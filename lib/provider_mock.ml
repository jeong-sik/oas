(** Mock provider for testing — eliminates network dependency.

    Supplies pre-programmed API responses in order. When the response
    list is exhausted the mock cycles back to the beginning.
    This replaces the Cohttp_eio.Server-based HTTP mocks used in
    test_handoff.ml and similar files. *)

open Types

(** A response function: given the conversation so far, produce a response. *)
type response_fn = message list -> api_response

(** Scripted mock: list of response functions applied in order.
    When exhausted, wraps around from the beginning. *)
type t = {
  responses: response_fn list;
  mutable index: int;
}

let create ~responses () =
  { responses; index = 0 }

(** Get the next response for the given messages. *)
let next_response mock messages =
  match mock.responses with
  | [] ->
    Error (Error.Internal "Provider_mock: empty response list")
  | _ ->
    let n = List.length mock.responses in
    let fn = List.nth mock.responses (mock.index mod n) in
    mock.index <- mock.index + 1;
    Ok (fn messages)

(** Reset the mock index to replay from the beginning. *)
let reset mock =
  mock.index <- 0

(** How many responses have been consumed so far. *)
let call_count mock = mock.index

(* ── Convenience builders ─────────────────────────────────────── *)

(** Build a simple text response. *)
let text_response ?(id="mock-id") ?(model="mock-model") text =
  fun (_messages : message list) ->
    {
      id;
      model;
      stop_reason = EndTurn;
      content = [Text text];
      usage = Some {
        input_tokens = 100;
        output_tokens = 50;
        cache_creation_input_tokens = 0;
        cache_read_input_tokens = 0;
        cost_usd = None
      };
    }

(** Build a tool-use response. *)
let tool_use_response ?(id="mock-id") ?(model="mock-model")
    ~tool_name ~tool_input () =
  fun (_messages : message list) ->
    let tool_use_id = Printf.sprintf "toolu_%s_%d"
      tool_name (int_of_float (Unix.gettimeofday () *. 1000.0)) in
    {
      id;
      model;
      stop_reason = StopToolUse;
      content = [ToolUse { id = tool_use_id; name = tool_name; input = tool_input }];
      usage = Some {
        input_tokens = 150;
        output_tokens = 80;
        cache_creation_input_tokens = 0;
        cache_read_input_tokens = 0;
        cost_usd = None
      };
    }

(** Build a response that uses a tool then ends with text on the next call. *)
let tool_then_text ~tool_name ~tool_input ~final_text () =
  [
    tool_use_response ~tool_name ~tool_input ();
    text_response final_text;
  ]

(** Build a response with thinking block followed by text. *)
let thinking_response ?(id="mock-id") ?(model="mock-model")
    ~thinking ~text () =
  fun (_messages : message list) ->
    {
      id;
      model;
      stop_reason = EndTurn;
      content = [
        Thinking { thinking_type = "thinking"; content = thinking };
        Text text;
      ];
      usage = Some {
        input_tokens = 200;
        output_tokens = 150;
        cache_creation_input_tokens = 0;
        cache_read_input_tokens = 0;
        cost_usd = None
      };
    }

(** Create a Provider.config that routes through this mock.
    The mock replaces the HTTP layer — Api.create_message will
    call the provider's endpoint, but in tests we intercept earlier.

    NOTE: This returns a Provider.config for identification purposes.
    Actual interception happens in Harness.run_case which replaces
    the agent's run loop with mock-driven execution. *)
let to_provider_config () : Provider.config =
  {
    provider = Provider.Local { base_url = "http://mock:0/v1" };
    model_id = "mock-model";
    api_key_env = "MOCK_API_KEY";
  }
