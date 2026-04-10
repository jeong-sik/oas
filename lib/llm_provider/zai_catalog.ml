(** Z.AI catalog and routing helpers. *)

type api_mode =
  | General_api
  | Coding_plan

let general_base_url = "https://api.z.ai/api/paas/v4"
let coding_base_url = "https://api.z.ai/api/coding/paas/v4"
let anthropic_base_url = "https://api.z.ai/api/anthropic"

let is_glm_model_id model_id =
  let m = String.lowercase_ascii (String.trim model_id) in
  m = "glm"
  || (String.length m > 4 && String.sub m 0 4 = "glm-")

let contains_substring haystack needle =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  let rec loop i =
    if i + nlen > hlen then false
    else if String.sub haystack i nlen = needle then true
    else loop (i + 1)
  in
  nlen > 0 && loop 0

let is_coding_base_url base_url =
  contains_substring base_url "/api/coding/paas/"

let is_anthropic_base_url base_url =
  contains_substring base_url "/api/anthropic"

let mode_of_base_url base_url =
  if is_coding_base_url base_url || is_anthropic_base_url base_url then
    Coding_plan
  else
    General_api

let split_csv value =
  String.split_on_char ',' value
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")

let glm_auto_models () =
  match Sys.getenv_opt "ZAI_AUTO_MODELS" with
  | Some v when String.trim v <> "" -> split_csv v
  | _ -> [ "glm-5.1"; "glm-5-turbo"; "glm-4.7"; "glm-4.7-flashx" ]

let glm_coding_auto_models () =
  match Sys.getenv_opt "ZAI_CODING_AUTO_MODELS" with
  | Some v when String.trim v <> "" -> split_csv v
  | _ -> [ "glm-4.7"; "glm-5-turbo"; "glm-5.1"; "glm-4.5-air" ]

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

let resolve_glm_coding_alias ~default_model model_id =
  resolve_glm_alias ~default_model model_id

let general_concurrency_for_model model_id =
  let m = String.lowercase_ascii model_id in
  let starts_with prefix =
    String.length m >= String.length prefix
    && String.sub m 0 (String.length prefix) = prefix
  in
  if starts_with "glm-4-plus" then 20
  else if starts_with "glm-4-32b-0414-128k" then 15
  else if starts_with "glm-4.5" && not (starts_with "glm-4.5-flash") then 10
  else if starts_with "glm-4.6v" then
    if starts_with "glm-4.6v-flashx" then 3
    else if starts_with "glm-4.6v-flash" then 1
    else 10
  else if starts_with "glm-4.6" then 3
  else if starts_with "glm-4.7-flashx" then 3
  else if starts_with "glm-4.7-flash" then 1
  else if starts_with "glm-4.7" then 2
  else if starts_with "glm-5v-turbo" then 1
  else if starts_with "glm-5.1" then 1
  else if starts_with "glm-5-turbo" then 1
  else if starts_with "glm-5" then 2
  else if starts_with "glm-ocr" then 2
  else 1

let coding_concurrency_default = 1

let throttle_key_for_chat ~base_url ~model_id =
  match mode_of_base_url base_url with
  | Coding_plan -> "zai/coding/chat"
  | General_api ->
      Printf.sprintf "zai/general/chat/%s"
        (String.lowercase_ascii (String.trim model_id))
