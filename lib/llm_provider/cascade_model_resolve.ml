(** Model ID resolution: aliases and auto-detection for cloud providers.

    Pure functions that map user-facing aliases to concrete API model IDs.
    No side effects beyond reading environment variables.

    @since 0.92.0 extracted from Cascade_config *)

(* ── GLM model catalog ──────────────────────────────── *)

(** Resolve GLM alias to concrete model ID.
    ZhipuAI serves all models on one endpoint; the "model" field
    must be an exact ID from their catalog.

    Catalog (2026-03, updated):
    {b Text}: glm-5.1, glm-5, glm-5-turbo, glm-4.7, glm-4.7-flashx,
              glm-4.6, glm-4.5, glm-4.5-air, glm-4.5-airx,
              glm-4.5-flash, glm-4.5-x, glm-4-32b-0414-128k
    {b Vision}: glm-4.6v, glm-4.6v-flashx, glm-4.6v-flash, glm-4.5v
    {b Audio}: glm-asr-2512
    {b Image gen}: cogview-4, glm-image

    All text/vision models support function calling.
    glm-5.1 supports reasoning (reasoning_content field). *)
let resolve_glm_model_id model_id =
  let env_or default var =
    match Sys.getenv_opt var with
    | Some m when String.trim m <> "" -> String.trim m
    | _ -> default
  in
  match String.lowercase_ascii model_id with
  (* aliases -> concrete IDs *)
  | "auto" -> env_or "glm-5.1" "ZAI_DEFAULT_MODEL"
  | "flash" -> "glm-4.7-flashx"
  | "turbo" -> "glm-5-turbo"
  | "vision" | "v" -> "glm-4.6v"
  | "vision-flash" | "vf" -> "glm-4.6v-flashx"
  | "air" -> "glm-4.5-air"
  | "ocr" -> "glm-ocr"
  (* already concrete -> pass through *)
  | _ -> model_id

(** Resolve "auto" and aliases to concrete model IDs.
    Cloud APIs reject unknown model names; local servers accept any. *)
let resolve_auto_model_id provider_name model_id =
  let env_or default var =
    match Sys.getenv_opt var with
    | Some m when String.trim m <> "" -> String.trim m
    | _ -> default
  in
  match provider_name with
  | "glm" -> resolve_glm_model_id model_id
  | "gemini" ->
    if model_id = "auto" then env_or "gemini-2.5-flash" "GEMINI_DEFAULT_MODEL"
    else model_id
  | "claude" ->
    if model_id = "auto" then env_or "claude-sonnet-4-6-20250514" "ANTHROPIC_DEFAULT_MODEL"
    else model_id
  | "openrouter" ->
    if model_id = "auto" then env_or model_id "OPENROUTER_DEFAULT_MODEL"
    else model_id
  | _ -> model_id

let parse_custom_model model_id =
  match String.index_opt model_id '@' with
  | Some at_idx ->
    let model = String.sub model_id 0 at_idx in
    let url = String.sub model_id (at_idx + 1) (String.length model_id - at_idx - 1) in
    (model, url)
  | None ->
    let url =
      match Sys.getenv_opt "CUSTOM_LLM_BASE_URL" with
      | Some u -> u
      | None -> "http://127.0.0.1:8080"
    in
    (model_id, url)
