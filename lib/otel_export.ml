(** OTLP HTTP/JSON span exporter.

    Uses cohttp-eio directly for HTTP transport.
    No dependency on {!Llm_provider.Http_client}.

    @since 0.102.0 *)

type export_config = {
  endpoint: string;
  headers: (string * string) list;
  flush_interval_sec: float;
  max_batch_size: int;
  max_retries: int;
  timeout_sec: float;
}

let default_export_config ~endpoint = {
  endpoint;
  headers = [];
  flush_interval_sec = 5.0;
  max_batch_size = 512;
  max_retries = 3;
  timeout_sec = 10.0;
}

type export_result =
  | Exported of { span_count: int }
  | Partial_failure of { exported: int; dropped: int; reason: string }
  | Failed of { reason: string }

(* ── TLS helper (self-contained, no llm_provider dep) ───────── *)

let make_https () =
  match Ca_certs.authenticator () with
  | Error _ -> None
  | Ok authenticator ->
    match Tls.Config.client ~authenticator () with
    | Error _ -> None
    | Ok tls_config ->
      Some (fun _uri socket ->
        Tls_eio.client_of_flow tls_config socket)

let make_client ~net =
  Cohttp_eio.Client.make ~https:(make_https ()) net

(* ── OTLP JSON body construction ────────────────────────────── *)

let build_otlp_body ~service_name (spans : Otel_tracer.span list) : string =
  let json = `Assoc [
    ("resourceSpans", `List [
      `Assoc [
        ("resource", `Assoc [
          ("attributes", Otel_tracer.attrs_to_json [
            ("service.name", service_name)
          ])
        ]);
        ("scopeSpans", `List [
          `Assoc [
            ("scope", `Assoc [
              ("name", `String "agent_sdk.otel_export");
              ("version", `String Sdk_version.version);
            ]);
            ("spans", `List (List.map Otel_tracer.span_to_json spans));
          ]
        ])
      ]
    ])
  ] in
  Yojson.Safe.to_string json

(* ── HTTP POST via cohttp-eio ───────────────────────────────── *)

let post_otlp ~sw ~client ~config body =
  let uri = Uri.of_string config.endpoint in
  let base_headers =
    Cohttp.Header.of_list
      ([ ("content-type", "application/json") ] @ config.headers)
  in
  let body_s = Cohttp_eio.Body.of_string body in
  try
    let resp, resp_body =
      Cohttp_eio.Client.post ~sw ~headers:base_headers ~body:body_s
        client uri
    in
    let status = Cohttp.Response.status resp in
    let code = Cohttp.Code.code_of_status status in
    let _ = Eio.Buf_read.of_flow ~max_size:(1024 * 64) resp_body
            |> Eio.Buf_read.take_all in
    if code >= 200 && code < 300 then Ok ()
    else Error (Printf.sprintf "HTTP %d" code)
  with
  | exn -> Error (Printexc.to_string exn)

(* ── Batch + retry logic ────────────────────────────────────── *)

let export_batch ~sw ~client ~config ~service_name spans =
  let body = build_otlp_body ~service_name spans in
  let rec attempt n =
    match post_otlp ~sw ~client ~config body with
    | Ok () -> Ok (List.length spans)
    | Error _ when n < config.max_retries ->
      Unix.sleepf (Float.pow 2.0 (Float.of_int n) *. 0.5);
      attempt (n + 1)
    | Error reason -> Error reason
  in
  attempt 0

let take_batch max_size lst =
  let rec go n acc rest =
    match rest with
    | [] -> (List.rev acc, [])
    | _ when n <= 0 -> (List.rev acc, rest)
    | x :: xs -> go (n - 1) (x :: acc) xs
  in
  go max_size [] lst

let rec split_batches max_size acc = function
  | [] -> List.rev acc
  | spans ->
    let batch, rest = take_batch max_size spans in
    split_batches max_size (batch :: acc) rest

let flush_to_collector ~sw ~net ~config instance =
  let spans = Otel_tracer.inst_flush instance in
  if spans = [] then Exported { span_count = 0 }
  else
    let client = make_client ~net in
    let service_name = instance.Otel_tracer.config.service_name in
    let batches = split_batches config.max_batch_size [] spans in
    let total = List.length spans in
    let exported = ref 0 in
    let last_error = ref "" in
    List.iter (fun batch ->
      match export_batch ~sw ~client ~config ~service_name batch with
      | Ok count -> exported := !exported + count
      | Error reason -> last_error := reason
    ) batches;
    if !exported = total then Exported { span_count = total }
    else if !exported > 0 then
      Partial_failure {
        exported = !exported;
        dropped = total - !exported;
        reason = !last_error;
      }
    else Failed { reason = !last_error }

(* ── Background daemon ──────────────────────────────────────── *)

type t = {
  config: export_config;
  instance: Otel_tracer.instance;
  mutable total_exported: int;
}

let start_daemon ~sw ~clock ~net ~config ?on_export instance =
  let state = {
    config;
    instance;
    total_exported = 0;
  } in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    while true do
      Eio.Time.sleep clock config.flush_interval_sec;
      let result = flush_to_collector ~sw ~net ~config instance in
      (match result with
       | Exported { span_count } ->
         state.total_exported <- state.total_exported + span_count
       | Partial_failure { exported; _ } ->
         state.total_exported <- state.total_exported + exported
       | Failed _ -> ());
      (match on_export with Some cb -> cb result | None -> ())
    done;
    `Stop_daemon
  );
  state

let force_flush ~sw ~net t =
  let result = flush_to_collector ~sw ~net ~config:t.config t.instance in
  (match result with
   | Exported { span_count } ->
     t.total_exported <- t.total_exported + span_count
   | Partial_failure { exported; _ } ->
     t.total_exported <- t.total_exported + exported
   | Failed _ -> ());
  result

let total_exported t = t.total_exported
