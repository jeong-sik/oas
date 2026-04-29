(** Z.AI catalog and routing helpers. *)

type api_mode =
  | General_api
  | Coding_plan

let general_base_url = "https://api.z.ai/api/paas/v4"
let coding_base_url = "https://api.z.ai/api/coding/paas/v4"
let anthropic_base_url = "https://api.z.ai/api/anthropic"

let is_glm_model_id model_id =
  let m = String.lowercase_ascii (String.trim model_id) in
  m = "glm" || (String.length m > 4 && String.sub m 0 4 = "glm-")
;;

let has_prefix value prefix =
  let vlen = String.length value in
  let plen = String.length prefix in
  plen <= vlen && String.sub value 0 plen = prefix
;;

let uri_host base_url =
  Uri.of_string base_url |> Uri.host |> Option.map String.lowercase_ascii
;;

let configured_base_urls defaults env_var =
  defaults
  @
  match env_var with
  | Some var ->
    (match Sys.getenv_opt var with
     | Some value ->
       let trimmed = String.trim value in
       if trimmed = "" then [] else [ trimmed ]
     | None -> [])
  | None -> []
;;

let base_url_matches_configured_base base_url configured_base_url =
  let uri = Uri.of_string base_url in
  let configured_uri = Uri.of_string configured_base_url in
  match uri_host base_url, uri_host configured_base_url with
  | Some host, Some configured_host when host = configured_host ->
    has_prefix (Uri.path uri) (Uri.path configured_uri)
  | _ -> false
;;

let zai_path_prefix_matches configured_base_urls base_url path_prefix =
  let uri = Uri.of_string base_url in
  has_prefix (Uri.path uri) path_prefix
  && List.exists (base_url_matches_configured_base base_url) configured_base_urls
;;

let is_coding_base_url base_url =
  zai_path_prefix_matches
    (configured_base_urls [ coding_base_url ] (Some "ZAI_CODING_BASE_URL"))
    base_url
    "/api/coding/paas/"
;;

let is_anthropic_base_url base_url =
  zai_path_prefix_matches
    (configured_base_urls [ anthropic_base_url ] None)
    base_url
    "/api/anthropic"
;;

let is_zai_base_url base_url =
  zai_path_prefix_matches
    (configured_base_urls [ general_base_url ] (Some "ZAI_BASE_URL"))
    base_url
    "/api/paas/"
  || is_coding_base_url base_url
  || is_anthropic_base_url base_url
;;

let mode_of_base_url base_url =
  if is_coding_base_url base_url || is_anthropic_base_url base_url
  then Coding_plan
  else General_api
;;

let split_csv value =
  String.split_on_char ',' value |> List.map String.trim |> List.filter (fun s -> s <> "")
;;

let glm_auto_models () =
  match Sys.getenv_opt "ZAI_AUTO_MODELS" with
  | Some v when String.trim v <> "" -> split_csv v
  | _ -> [ "glm-5.1"; "glm-5-turbo"; "glm-4.7"; "glm-4.7-flashx" ]
;;

let glm_coding_auto_models () =
  match Sys.getenv_opt "ZAI_CODING_AUTO_MODELS" with
  | Some v when String.trim v <> "" -> split_csv v
  | _ -> [ "glm-5.1"; "glm-5"; "glm-5-turbo"; "glm-4.7"; "glm-4.5-air" ]
;;

let resolve_glm_alias ~default_model model_id =
  match String.lowercase_ascii model_id with
  | "auto" -> default_model
  | "flash" -> "glm-4.7-flashx"
  | "turbo" -> "glm-5-turbo"
  | "vision" | "v" -> "glm-4.6v"
  | "vision-flash" | "vf" -> "glm-4.6v-flashx"
  | "air" -> "glm-4.5-air"
  | "ocr" -> "glm-ocr"
  | _ -> model_id
;;

let resolve_glm_coding_alias ~default_model model_id =
  resolve_glm_alias ~default_model model_id
;;

let general_concurrency_for_model model_id =
  let m = String.lowercase_ascii model_id in
  let starts_with prefix =
    String.length m >= String.length prefix
    && String.sub m 0 (String.length prefix) = prefix
  in
  if starts_with "glm-4-plus"
  then 20
  else if starts_with "glm-4-32b-0414-128k"
  then 15
  else if starts_with "glm-4.5" && not (starts_with "glm-4.5-flash")
  then 10
  else if starts_with "glm-4.6v"
  then
    if starts_with "glm-4.6v-flashx"
    then 3
    else if starts_with "glm-4.6v-flash"
    then 1
    else 10
  else if starts_with "glm-4.6"
  then 3
  else if starts_with "glm-4.7-flashx"
  then 3
  else if starts_with "glm-4.7-flash"
  then 1
  else if starts_with "glm-4.7"
  then 2
  else if starts_with "glm-5v-turbo"
  then 1
  else if starts_with "glm-5.1"
  then 1
  else if starts_with "glm-5-turbo"
  then 1
  else if starts_with "glm-5"
  then 2
  else if starts_with "glm-ocr"
  then 2
  else 1
;;

let coding_concurrency_default = 3

let throttle_key_for_chat ~base_url ~model_id =
  match mode_of_base_url base_url with
  | Coding_plan -> "zai/coding/chat"
  | General_api ->
    Printf.sprintf "zai/general/chat/%s" (String.lowercase_ascii (String.trim model_id))
;;

[@@@coverage off]

let%test "is_glm_model_id accepts glm prefixes only" =
  is_glm_model_id "glm-5" && is_glm_model_id "glm" && not (is_glm_model_id "gpt-5")
;;

let%test "base_url classifiers distinguish general coding and anthropic" =
  is_zai_base_url general_base_url
  && is_zai_base_url coding_base_url
  && is_zai_base_url anthropic_base_url
  && is_coding_base_url coding_base_url
  && (not (is_coding_base_url general_base_url))
  && is_anthropic_base_url anthropic_base_url
  && not (is_anthropic_base_url general_base_url)
;;

let%test "mode_of_base_url maps coding and anthropic to coding plan" =
  mode_of_base_url general_base_url = General_api
  && mode_of_base_url coding_base_url = Coding_plan
  && mode_of_base_url anthropic_base_url = Coding_plan
;;

let%test "resolve_glm_alias covers common aliases" =
  resolve_glm_alias ~default_model:"glm-5.1" "auto" = "glm-5.1"
  && resolve_glm_alias ~default_model:"glm-5.1" "flash" = "glm-4.7-flashx"
  && resolve_glm_alias ~default_model:"glm-5.1" "vf" = "glm-4.6v-flashx"
  && resolve_glm_alias ~default_model:"glm-5.1" "air" = "glm-4.5-air"
  && resolve_glm_alias ~default_model:"glm-5.1" "glm-5-turbo" = "glm-5-turbo"
;;

let%test "general_concurrency_for_model hits key glm families" =
  general_concurrency_for_model "glm-4.5" = 10
  && general_concurrency_for_model "glm-4.5-flash" = 1
  && general_concurrency_for_model "glm-4.6v-flashx" = 3
  && general_concurrency_for_model "glm-4.7" = 2
  && general_concurrency_for_model "glm-5v-turbo" = 1
  && general_concurrency_for_model "glm-ocr" = 2
  && general_concurrency_for_model "unknown-model" = 1
;;

let%test "throttle_key_for_chat separates coding and general plans" =
  throttle_key_for_chat ~base_url:general_base_url ~model_id:" GLM-5 "
  = "zai/general/chat/glm-5"
  && throttle_key_for_chat ~base_url:coding_base_url ~model_id:"glm-5" = "zai/coding/chat"
;;

let%test "is_zai_base_url rejects untrusted host lookalikes" =
  not (is_zai_base_url "https://proxy.example.com/api/paas/v4")
;;

let%test "is_zai_base_url accepts official endpoint" = is_zai_base_url general_base_url

let%test "mode_of_base_url keeps coding endpoint on coding plan" =
  mode_of_base_url coding_base_url = Coding_plan
;;
