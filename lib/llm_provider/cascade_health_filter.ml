(** Cascade health filtering — classify errors and filter provider lists
    by local health discovery and API key presence.

    Extracted from cascade_config.ml for cohesion.

    @since 0.99.5 *)

(* ── Cascade-level error classification ────────────────── *)

(** Decide whether an error should cascade to the next provider.
    Local resource exhaustion (port/FD limits) stops the cascade
    because every subsequent provider will hit the same bottleneck. *)
let should_cascade_to_next err =
  if Http_client.is_local_resource_exhaustion err then false
  else match err with
  | Http_client.HttpError { code; body }
    when List.mem code [400; 422]
         && Retry.is_context_overflow_message body ->
    true
  | Http_client.HttpError { code; _ } ->
    List.mem code Constants.Http.cascadable_codes
  | Http_client.AcceptRejected _ -> false
  | Http_client.NetworkError _ -> true

(* ── Local provider detection ──────────────────────────── *)

let is_local_provider (cfg : Provider_config.t) =
  let url = String.lowercase_ascii cfg.base_url in
  let len = String.length url in
  let starts_with prefix =
    let plen = String.length prefix in
    len >= plen && String.sub url 0 plen = prefix
  in
  starts_with "http://127.0.0.1"
  || starts_with "http://localhost:"
  || starts_with "http://localhost/"
  || url = "http://localhost"

(** Check whether a provider has credentials when required. *)
let has_required_api_key (cfg : Provider_config.t) =
  cfg.api_key <> "" || is_local_provider cfg

(* ── Discovery-aware health filtering ──────────────────── *)

(** Internal: filter healthy + return discovery statuses for throttle. *)
let filter_healthy_internal ~sw ~net (providers : Provider_config.t list) =
  (* Step 0: Remove cloud providers missing required API keys *)
  let providers =
    let with_keys = List.filter has_required_api_key providers in
    if with_keys = [] then providers  (* keep all rather than empty *)
    else with_keys
  in
  let local_providers =
    List.filter is_local_provider providers
  in
  let cloud_providers =
    List.filter (fun cfg -> not (is_local_provider cfg)) providers
  in
  if local_providers = [] then
    (providers, [])
  else
    let endpoints =
      local_providers
      |> List.map (fun (cfg : Provider_config.t) -> cfg.base_url)
      |> List.sort_uniq String.compare
    in
    (* Use refresh_and_sync instead of discover so that the shared
       model_endpoints index is populated. Without this, endpoint_for_model
       always returns None and model-specific routing in
       make_registry_config falls back to round-robin. See #677. *)
    let statuses = Discovery.refresh_and_sync ~sw ~net ~endpoints in
    if cloud_providers = [] then
      (providers, statuses)
    else
      let any_healthy =
        List.exists (fun (s : Discovery.endpoint_status) -> s.healthy) statuses
      in
      if any_healthy then
        (providers, statuses)
      else
        (cloud_providers, [])

let filter_healthy ~sw ~net providers =
  fst (filter_healthy_internal ~sw ~net providers)

(* ── Inline tests ──────────────────────────────────────── *)

[@@@coverage off]

let%test "should_cascade_to_next 401 auth error" =
  should_cascade_to_next (Http_client.HttpError { code = 401; body = "" }) = true

let%test "should_cascade_to_next 403 forbidden" =
  should_cascade_to_next (Http_client.HttpError { code = 403; body = "" }) = true

let%test "should_cascade_to_next 429 rate limit" =
  should_cascade_to_next (Http_client.HttpError { code = 429; body = "" }) = true

let%test "should_cascade_to_next 500 server error" =
  should_cascade_to_next (Http_client.HttpError { code = 500; body = "" }) = true

let%test "should_cascade_to_next 502 bad gateway" =
  should_cascade_to_next (Http_client.HttpError { code = 502; body = "" }) = true

let%test "should_cascade_to_next 503 service unavailable" =
  should_cascade_to_next (Http_client.HttpError { code = 503; body = "" }) = true

let%test "should_cascade_to_next 529 overloaded" =
  should_cascade_to_next (Http_client.HttpError { code = 529; body = "" }) = true

let%test "should_cascade_to_next network error" =
  should_cascade_to_next (Http_client.NetworkError { message = "refused" }) = true

let%test "should_cascade_to_next EADDRNOTAVAIL stops" =
  should_cascade_to_next (Http_client.NetworkError {
    message = "Eio.Io Unix_error (Can't assign requested address, \"connect\", \"\"), connecting to tcp:128.14.69.121:443"
  }) = false

let%test "should_cascade_to_next EMFILE stops" =
  should_cascade_to_next (Http_client.NetworkError {
    message = "Unix.Unix_error(Unix.EMFILE, \"socket\", \"\")"
  }) = false

let%test "should_cascade_to_next too many open files stops" =
  should_cascade_to_next (Http_client.NetworkError {
    message = "Too many open files"
  }) = false

let%test "should_cascade_to_next 400 bad request stops" =
  should_cascade_to_next (Http_client.HttpError { code = 400; body = "" }) = false

let%test "should_cascade_to_next overflow 400 cascades" =
  should_cascade_to_next
    (Http_client.HttpError {
       code = 400;
       body =
         {|{"error":{"message":"request (11447 tokens) exceeds the available context size (8192 tokens), try increasing it"}}|};
    }) = true

let%test "should_cascade_to_next 404 not found stops" =
  should_cascade_to_next (Http_client.HttpError { code = 404; body = "" }) = false

let%test "has_required_api_key with key present" =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:"https://api.example.com" ~api_key:"sk-123" () in
  has_required_api_key cfg = true

let%test "has_required_api_key local no key is ok" =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:Constants.Endpoints.default_url ~api_key:"" () in
  has_required_api_key cfg = true

let%test "has_required_api_key cloud no key fails" =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:"https://api.example.com" ~api_key:"" () in
  has_required_api_key cfg = false

let%test "has_required_api_key localhost with port is local" =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:"http://localhost:11434" ~api_key:"" () in
  has_required_api_key cfg = true
