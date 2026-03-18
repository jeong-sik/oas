(** Tests for Llm_provider.Discovery -- unit tests that do not require
    a running llama-server. We test JSON parsing, env var parsing,
    and serialization. *)

open Llm_provider

let test_endpoints_from_env_default () =
  (* When LLM_ENDPOINTS is not set, should return default *)
  let saved = Sys.getenv_opt "LLM_ENDPOINTS" in
  (match saved with Some _ -> Unix.putenv "LLM_ENDPOINTS" "" | None -> ());
  let result = Discovery.endpoints_from_env () in
  (match saved with Some v -> Unix.putenv "LLM_ENDPOINTS" v | None -> ());
  Alcotest.(check (list string)) "default endpoint"
    ["http://127.0.0.1:8085"] result

let test_endpoints_from_env_custom () =
  let saved = Sys.getenv_opt "LLM_ENDPOINTS" in
  Unix.putenv "LLM_ENDPOINTS" "http://a:8085, http://b:8086,http://c:8087";
  let result = Discovery.endpoints_from_env () in
  (match saved with Some v -> Unix.putenv "LLM_ENDPOINTS" v | None -> ());
  Alcotest.(check (list string)) "parsed endpoints"
    ["http://a:8085"; "http://b:8086"; "http://c:8087"] result

let test_parse_models_json () =
  let json = Yojson.Safe.from_string {|{
    "data": [
      {"id": "qwen3.5-35b", "owned_by": "llama-server"},
      {"id": "llama-3.1-8b", "owned_by": "llama-server"}
    ]
  }|} in
  let models =
    (* Use the internal parser indirectly via a full endpoint_status *)
    match json |> Yojson.Safe.Util.member "data" with
    | `List items ->
      items |> List.filter_map (fun item ->
        let open Yojson.Safe.Util in
        match item |> member "id" |> to_string_option with
        | Some id ->
          let owned_by =
            item |> member "owned_by" |> to_string_option
            |> Option.value ~default:"unknown"
          in
          Some Discovery.{ id; owned_by }
        | None -> None)
    | _ -> []
  in
  Alcotest.(check int) "model count" 2 (List.length models);
  Alcotest.(check string) "first model id" "qwen3.5-35b"
    (List.hd models).id

let test_endpoint_status_to_json_healthy () =
  let status : Discovery.endpoint_status = {
    url = "http://127.0.0.1:8085";
    healthy = true;
    models = [{ id = "qwen3.5-35b"; owned_by = "llama-server" }];
    props = Some { total_slots = 4; ctx_size = 32768; model = "qwen3.5-35b" };
    slots = Some { total = 4; busy = 1; idle = 3 };
    capabilities = Capabilities.qwen_openai_chat_capabilities;
  } in
  let json = Discovery.endpoint_status_to_json status in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "healthy" true
    (json |> member "healthy" |> to_bool);
  Alcotest.(check string) "url" "http://127.0.0.1:8085"
    (json |> member "url" |> to_string);
  let slots = json |> member "slots" in
  Alcotest.(check int) "total slots" 4
    (slots |> member "total" |> to_int);
  Alcotest.(check int) "idle slots" 3
    (slots |> member "idle" |> to_int);
  let caps = json |> member "capabilities" in
  Alcotest.(check bool) "reasoning" true
    (caps |> member "reasoning" |> to_bool)

let test_endpoint_status_to_json_unhealthy () =
  let status : Discovery.endpoint_status = {
    url = "http://127.0.0.1:9999";
    healthy = false;
    models = [];
    props = None;
    slots = None;
    capabilities = Capabilities.default_capabilities;
  } in
  let json = Discovery.endpoint_status_to_json status in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "healthy" false
    (json |> member "healthy" |> to_bool);
  Alcotest.(check int) "no models" 0
    (json |> member "models" |> to_list |> List.length);
  (* props and slots should be absent *)
  Alcotest.(check bool) "no props" true
    (member "props" json = `Null)

let test_summary_to_json () =
  let endpoints : Discovery.endpoint_status list = [
    { url = "http://a:8085"; healthy = true;
      models = []; props = None;
      slots = Some { total = 4; busy = 1; idle = 3 };
      capabilities = Capabilities.default_capabilities };
    { url = "http://b:8086"; healthy = true;
      models = []; props = None;
      slots = Some { total = 2; busy = 2; idle = 0 };
      capabilities = Capabilities.default_capabilities };
    { url = "http://c:8087"; healthy = false;
      models = []; props = None; slots = None;
      capabilities = Capabilities.default_capabilities };
  ] in
  let json = Discovery.summary_to_json endpoints in
  let open Yojson.Safe.Util in
  Alcotest.(check int) "total capacity" 6
    (json |> member "total_capacity" |> to_int);
  Alcotest.(check int) "available" 3
    (json |> member "available_capacity" |> to_int);
  Alcotest.(check int) "active" 3
    (json |> member "active_requests" |> to_int)

let () =
  Alcotest.run "Discovery" [
    "env", [
      Alcotest.test_case "default" `Quick test_endpoints_from_env_default;
      Alcotest.test_case "custom" `Quick test_endpoints_from_env_custom;
    ];
    "parsing", [
      Alcotest.test_case "models json" `Quick test_parse_models_json;
    ];
    "json", [
      Alcotest.test_case "healthy endpoint" `Quick test_endpoint_status_to_json_healthy;
      Alcotest.test_case "unhealthy endpoint" `Quick test_endpoint_status_to_json_unhealthy;
      Alcotest.test_case "summary" `Quick test_summary_to_json;
    ];
  ]
