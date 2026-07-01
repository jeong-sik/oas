(** Per-model cost estimation.

    Extracted from OAS Provider module. Provides pricing lookup by model ID
    and USD cost calculation from token counts.

    @since 0.42.0 *)

type pricing =
  { input_per_million : float
  ; output_per_million : float
  ; cache_write_multiplier : float (** cache creation tokens cost input_rate * this *)
  ; cache_read_multiplier : float (** cache read tokens cost input_rate * this *)
  }

(** A runtime override entry mapping a substring pattern to pricing.
    Loaded from [OAS_PRICING_FILE] or installed via [install_pricing_overrides]. *)
type pricing_entry =
  { pattern : string
  ; input_per_million : float
  ; output_per_million : float
  ; cache_write_multiplier : float
  ; cache_read_multiplier : float
  }

(* ── Dynamic override table ──────────────────────────────────── *)

(** Process-wide pricing overrides: [(loaded_at_unix_s, entries) option].
    [None] means no overrides installed — fall through to static table. *)
let _overrides : (float * pricing_entry list) option Atomic.t = Atomic.make None

(** Guards against repeated per-process staleness log spam. *)
let _staleness_warned : bool Atomic.t = Atomic.make false

(** TTL for loaded pricing data: 24 hours. *)
let pricing_ttl_s = 86400.0

(** Return the current override entries, emitting a one-shot [Diag.warn] when
    the table is older than [pricing_ttl_s] (24 h). *)
let _get_overrides () =
  match Atomic.get _overrides with
  | None -> []
  | Some (loaded_at, entries) ->
    let age = Unix.gettimeofday () -. loaded_at in
    if age > pricing_ttl_s && Atomic.compare_and_set _staleness_warned false true
    then
      Diag.warn
        "pricing"
        "pricing overrides are %.1f h old (TTL=24 h); refresh via OAS_PRICING_FILE or \
         install_pricing_overrides"
        (age /. 3600.0);
    entries
;;

let string_contains ~needle haystack =
  needle = ""
  ||
  try
    let (_ : int) = Str.search_forward (Str.regexp_string needle) haystack 0 in
    true
  with
  | Not_found -> false
;;

(* Strip an OpenRouter-style provider/org prefix so that a model id such as
   ["anthropic/claude-sonnet-4-6"] can be matched against catalog prefixes that
   omit the organization. *)
let provider_suffix model_id =
  match String.index_opt model_id '/' with
  | Some i when i + 1 < String.length model_id ->
    Some (String.sub model_id (i + 1) (String.length model_id - i - 1))
  | _ -> None
;;

let starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix
;;

(* Model-id component separators. ["_"] is included because route ids such as
   the in-tree DashScope catalog entry ["dashscope_3"] use it as a boundary, so a
   prefix like ["dashscope"] must anchor on it the same way it does on ["-"].
   Codex P2 on #2127. *)
let is_model_delimiter = function
  | '-' | ':' | '.' | '/' | '_' -> true
  | _ -> false
;;

let delimited_prefix_match ~prefix s =
  starts_with ~prefix s
  &&
  let prefix_len = String.length prefix in
  String.length s = prefix_len
  || (String.length s > prefix_len && is_model_delimiter s.[prefix_len])
;;

let zero_pricing : pricing =
  { input_per_million = 0.0
  ; output_per_million = 0.0
  ; cache_write_multiplier = 1.0
  ; cache_read_multiplier = 1.0
  }
;;

type static_match_kind =
  | Exact
  | Delimited_prefix
  | Raw_prefix

type static_pricing_entry =
  { key : string
  ; match_kind : static_match_kind
  ; pricing : pricing option
  }

let static_entry ?(match_kind = Delimited_prefix) key pricing =
  { key; match_kind; pricing }
;;

let static_entry_matches entry normalized =
  match entry.match_kind with
  | Exact -> normalized = entry.key
  | Delimited_prefix -> delimited_prefix_match ~prefix:entry.key normalized
  | Raw_prefix -> starts_with ~prefix:entry.key normalized
;;

let static_free_alias_matches normalized =
  let exact_aliases = [ "auto"; "gemini"; "kimi"; "codex" ] in
  let prefix_aliases = [ "ollama"; "dashscope"; "nous" ] in
  List.exists (String.equal normalized) exact_aliases
  || List.exists (fun prefix -> delimited_prefix_match ~prefix normalized) prefix_aliases
;;

let catalog_pricing_entry_matches ~id_prefix normalized =
  let prefix = String.lowercase_ascii (String.trim id_prefix) in
  if prefix = ""
  then false
  else if prefix = "gpt"
  then normalized = "gpt"
  else if is_model_delimiter prefix.[String.length prefix - 1]
  then
    (* A prefix that already ends in a delimiter (e.g. "cc:" or a provider
       namespace "myorg/") is a raw prefix: requiring another delimiter after it
       would reject the very ids it is meant to price. Codex P2 on #2127. *)
    starts_with ~prefix normalized
  else delimited_prefix_match ~prefix normalized
;;

(* Built-in fallback pricing table. Used when the external model catalog is
   unavailable or does not contain a matching entry. This restores the
   previously in-code pricing knowledge for the most common cloud models,
   ordered by descending key length so longer keys shadow shorter ones. Static
   matching is exact or delimiter-anchored; it intentionally does not use
   substring matching, so unknown future families such as ["gpt-6-turbo"] do
   not inherit the bare ["gpt"] price. *)
let static_pricing_entries =
  let anthropic_cache = 1.25, 0.1 in
  let openai_cached_input = 1.0, 0.1 in
  let no_cache = 1.0, 1.0 in
  let make ?(cache = no_cache) input output =
    let cw, cr = cache in
    Some
      { input_per_million = input
      ; output_per_million = output
      ; cache_write_multiplier = cw
      ; cache_read_multiplier = cr
      }
  in
  let entries =
    (* [gpt-5.3-codex-spark] is an explicit no-pricing sentinel. Delimited_prefix
       (not Exact) so spark variants such as gpt-5.3-codex-spark-next also stay
       unpriced instead of falling through to the broader gpt-5.3-codex entry. *)
    [ static_entry ~match_kind:Delimited_prefix "gpt-5.3-codex-spark" None
    ; static_entry "claude-opus-4-6" (make ~cache:anthropic_cache 15.0 75.0)
    ; static_entry "claude-opus-4-5" (make ~cache:anthropic_cache 15.0 75.0)
    ; static_entry "claude-opus-4" (make ~cache:anthropic_cache 15.0 75.0)
    ; static_entry "claude-sonnet-4-6" (make ~cache:anthropic_cache 3.0 15.0)
    ; static_entry "claude-sonnet-4" (make ~cache:anthropic_cache 3.0 15.0)
    ; static_entry "claude-haiku-4-5" (make ~cache:anthropic_cache 0.8 4.0)
    ; static_entry "claude-haiku-4" (make ~cache:anthropic_cache 0.8 4.0)
    ; static_entry "claude-3-7-sonnet" (make ~cache:anthropic_cache 3.0 15.0)
    ; static_entry "claude_code" (make ~cache:anthropic_cache 3.0 15.0)
    ; static_entry ~match_kind:Raw_prefix "cc:" (make ~cache:anthropic_cache 3.0 15.0)
    ; static_entry "opus-4-6" (make ~cache:anthropic_cache 15.0 75.0)
    ; static_entry "opus-4-5" (make ~cache:anthropic_cache 15.0 75.0)
    ; static_entry "sonnet-4-6" (make ~cache:anthropic_cache 3.0 15.0)
    ; static_entry "sonnet-4" (make ~cache:anthropic_cache 3.0 15.0)
    ; static_entry "haiku-4-5" (make ~cache:anthropic_cache 0.8 4.0)
    ; static_entry "gpt-5.5" (make ~cache:openai_cached_input 5.0 30.0)
    ; static_entry "gpt-5.4-mini" (make ~cache:openai_cached_input 0.75 4.5)
    ; static_entry "gpt-5.4" (make ~cache:openai_cached_input 2.5 15.0)
    ; static_entry "gpt-5.3-codex" (make ~cache:openai_cached_input 1.75 14.0)
    ; static_entry "gpt-5.2" (make ~cache:openai_cached_input 1.75 14.0)
      (* Base gpt-5 family fallback (catalog gpt-5 = 5.0/30.0). Delimiter-prefix
         covers gpt-5 and gpt-5-latest; the more-specific gpt-5.x entries above
         are longer and win the length-sorted lookup. *)
    ; static_entry "gpt-5" (make ~cache:openai_cached_input 5.0 30.0)
    ; static_entry "gpt-4.1" (make 2.0 8.0)
      (* Known generic GPT aliases the repo still constructs (gpt-4, gpt-4o)
         plus the cheaper gpt-4o-mini. The typed Exact "gpt" no longer covers
         them, so enumerate them at their own rates instead of widening "gpt"
         back to a substring match (which would also price unknown future
         families like gpt-6-turbo). These use delimiter-prefix matching so
         dated ids (gpt-4o-2024-08-06, gpt-4-0613) price like their base; the
         table is sorted by descending key length, so the longer gpt-4o-mini is
         matched before gpt-4o and a mini id is never costed at the full gpt-4o
         rate. *)
    ; static_entry "gpt-4" (make 2.5 10.0)
    ; static_entry "gpt-4o" (make 2.5 10.0)
    ; static_entry "gpt-4o-mini" (make 0.15 0.6)
    ; static_entry "gpt-mini" (make 0.15 0.6)
    ; static_entry "o3-mini" (make 1.1 4.4)
    ; static_entry ~match_kind:Exact "gpt" (make 2.5 10.0)
    ]
  in
  List.sort (fun a b -> compare (String.length b.key) (String.length a.key)) entries
;;

(* Internal: static pricing table lookup on a pre-normalised model ID.
   Called by [pricing_for_model_opt] when no dynamic override or catalog entry
   matches. Free aliases are checked first, then the built-in paid-model
   fallback table. *)
let static_pricing_opt_normalized normalized =
  if static_free_alias_matches normalized
  then Some zero_pricing
  else (
    match
      List.find_opt
        (fun entry -> static_entry_matches entry normalized)
        static_pricing_entries
    with
    | Some entry -> entry.pricing
    | None -> None)
;;

(* The catalog gives three distinct answers, not two. Collapsing them into a
   [pricing option] conflates "no applicable entry" with "entry applies but is
   intentionally unpriced": the former must consult the static fallback, the
   latter must stay unpriced (consulting static there would override the
   operator's deliberate choice and fill in a price). Codex P2 on #2127. *)
type catalog_classification =
  | Catalog_priced of pricing
  | Catalog_unpriced (* applies to this id but deliberately omits a price *)
  | Catalog_no_match (* no applicable entry -> consult the static fallback *)

let catalog_classify catalog model_id =
  let normalized = String.lowercase_ascii (String.trim model_id) in
  match Model_catalog.lookup catalog model_id with
  | Some entry when catalog_pricing_entry_matches ~id_prefix:entry.id_prefix normalized ->
    (match entry.input_per_million, entry.output_per_million with
     | Some input, Some output ->
       Catalog_priced
         { input_per_million = input
         ; output_per_million = output
         ; cache_write_multiplier = Option.value entry.cache_write_multiplier ~default:1.0
         ; cache_read_multiplier = Option.value entry.cache_read_multiplier ~default:1.0
         }
     | _ -> Catalog_unpriced)
  | _ -> Catalog_no_match
;;

let catalog_pricing_opt catalog model_id =
  match catalog_classify catalog model_id with
  | Catalog_priced p -> Some p
  | Catalog_unpriced | Catalog_no_match -> None
;;

let pricing_for_model_opt model_id =
  let normalized = String.lowercase_ascii (String.trim model_id) in
  (* Check runtime overrides first (first pattern match wins). *)
  let overrides = _get_overrides () in
  let override_match =
    List.find_opt
      (fun e -> string_contains ~needle:(String.lowercase_ascii e.pattern) normalized)
      overrides
  in
  match override_match with
  | Some e ->
    Some
      { input_per_million = e.input_per_million
      ; output_per_million = e.output_per_million
      ; cache_write_multiplier = e.cache_write_multiplier
      ; cache_read_multiplier = e.cache_read_multiplier
      }
  | None ->
    (* Check the dynamic catalog and the built-in static fallback.
       Provider-prefixed model ids (e.g. ["anthropic/claude-sonnet-4-6"]) are
       tried both as-is and with the provider prefix stripped. *)
    let candidates =
      match provider_suffix model_id with
      | Some suffix when suffix <> model_id -> [ model_id; suffix ]
      | _ -> [ model_id ]
    in
    (* Classify against the catalog. The original id gets full three-valued
       treatment: a deliberate "unpriced" there must suppress the static
       fallback. Provider-stripped candidates are only a convenience for finding
       a PRICE -- a stripped id that is merely unpriced in the catalog must NOT
       suppress the static/free fallback for the original id (e.g.
       "dashscope/qwen3-32b" is free via the static dashscope alias even though
       the stripped "qwen3-32b" hits an unpriced catalog capability entry).
       Codex P2 on #2127. *)
    let catalog_class =
      match Model_catalog.global (), candidates with
      | None, _ | _, [] -> Catalog_no_match
      | Some catalog, original :: stripped ->
        (match catalog_classify catalog original with
         | (Catalog_priced _ | Catalog_unpriced) as definitive -> definitive
         | Catalog_no_match ->
           (match
              List.find_map
                (fun id ->
                   match catalog_classify catalog id with
                   | Catalog_priced p -> Some p
                   | Catalog_unpriced | Catalog_no_match -> None)
                stripped
            with
            | Some p -> Catalog_priced p
            | None -> Catalog_no_match))
    in
    (match catalog_class with
     | Catalog_priced p -> Some p
     | Catalog_unpriced -> None
     | Catalog_no_match ->
       List.find_map
         (fun id ->
            static_pricing_opt_normalized (String.lowercase_ascii (String.trim id)))
         candidates)
;;

let pricing_for_model model_id =
  Option.value ~default:zero_pricing (pricing_for_model_opt model_id)
;;

let estimate_cost
      ~(pricing : pricing)
      ~input_tokens
      ~output_tokens
      ?(cache_creation_input_tokens = 0)
      ?(cache_read_input_tokens = 0)
      ()
  =
  (* Regular input tokens (excluding cache tokens -- billed separately) *)
  let regular_input =
    input_tokens - cache_creation_input_tokens - cache_read_input_tokens
  in
  let regular_input = max 0 regular_input in
  let rate = pricing.input_per_million /. 1_000_000.0 in
  let input_cost = Float.of_int regular_input *. rate in
  let cache_write_cost =
    Float.of_int cache_creation_input_tokens *. rate *. pricing.cache_write_multiplier
  in
  let cache_read_cost =
    Float.of_int cache_read_input_tokens *. rate *. pricing.cache_read_multiplier
  in
  let output_cost =
    Float.of_int output_tokens *. pricing.output_per_million /. 1_000_000.0
  in
  input_cost +. cache_write_cost +. cache_read_cost +. output_cost
;;

let estimate_usage_cost ~model_id (usage : Types.api_usage) =
  let pricing = pricing_for_model model_id in
  estimate_cost
    ~pricing
    ~input_tokens:usage.input_tokens
    ~output_tokens:usage.output_tokens
    ~cache_creation_input_tokens:usage.cache_creation_input_tokens
    ~cache_read_input_tokens:usage.cache_read_input_tokens
    ()
;;

let annotate_usage_cost ~model_id (usage : Types.api_usage) =
  match usage.cost_usd with
  | Some _ -> usage
  | None ->
    (match pricing_for_model_opt model_id with
     | Some pricing ->
       let cost =
         estimate_cost
           ~pricing
           ~input_tokens:usage.input_tokens
           ~output_tokens:usage.output_tokens
           ~cache_creation_input_tokens:usage.cache_creation_input_tokens
           ~cache_read_input_tokens:usage.cache_read_input_tokens
           ()
       in
       { usage with cost_usd = Some cost }
     | None -> usage (* unknown model: leave cost_usd as None *))
;;

let annotate_response_cost (response : Types.api_response) =
  let usage = Option.map (annotate_usage_cost ~model_id:response.model) response.usage in
  match usage with
  | None -> response
  | Some usage -> { response with usage = Some usage }
;;

(* ── Dynamic override API ────────────────────────────────────── *)

(** Install [entries] as the process-wide pricing override table, resetting
    the loaded-at timestamp and clearing the staleness-warned flag. *)
let install_pricing_overrides entries =
  let loaded_at = Unix.gettimeofday () in
  Atomic.set _overrides (Some (loaded_at, entries));
  Atomic.set _staleness_warned false
;;

(** Remove all installed overrides; subsequent lookups use the static table. *)
let clear_pricing_overrides () =
  Atomic.set _overrides None;
  Atomic.set _staleness_warned false
;;

(** Parse a single pricing override entry from a JSON object.
    Required fields: [pattern], [input_per_million], [output_per_million].
    Optional fields: [cache_write_multiplier] (default 1.0),
    [cache_read_multiplier] (default 1.0). *)
let pricing_entry_of_json json =
  let open Yojson.Safe.Util in
  try
    let pattern = json |> member "pattern" |> to_string |> String.trim in
    if String.length pattern = 0
    then Error "\"pattern\" must be a non-empty string"
    else (
      let input_per_million = json |> member "input_per_million" |> to_float in
      let output_per_million = json |> member "output_per_million" |> to_float in
      let cache_write_multiplier =
        match json |> member "cache_write_multiplier" with
        | `Null -> 1.0
        | v -> to_float v
      in
      let cache_read_multiplier =
        match json |> member "cache_read_multiplier" with
        | `Null -> 1.0
        | v -> to_float v
      in
      Ok
        { pattern
        ; input_per_million
        ; output_per_million
        ; cache_write_multiplier
        ; cache_read_multiplier
        })
  with
  | Type_error (msg, _) -> Error ("type error: " ^ msg)
  | Not_found -> Error "missing required field"
;;

(** Parse a JSON array of pricing override entries.
    Returns [Error] if the top-level value is not an array, or if any
    entry is malformed (the error message lists all failures). *)
let parse_pricing_entries_json json =
  match json with
  | `List entries ->
    let results = List.map pricing_entry_of_json entries in
    let errors =
      List.filter_map
        (function
          | Error e -> Some e
          | Ok _ -> None)
        results
    in
    (match errors with
     | [] ->
       Ok
         (List.filter_map
            (function
              | Ok e -> Some e
              | Error _ -> None)
            results)
     | errs -> Error ("pricing entry parse errors: " ^ String.concat "; " errs))
  | _ -> Error "expected a JSON array of pricing entries"
;;

(** Load pricing overrides from a JSON file at [path].
    On success, installs the overrides and returns [Ok ()].
    On failure, leaves any existing overrides intact and returns [Error msg]. *)
let load_pricing_file path =
  match
    try Ok (Yojson.Safe.from_file path) with
    | Sys_error msg -> Error ("cannot read pricing file: " ^ msg)
    | Yojson.Json_error msg -> Error ("pricing file JSON parse error: " ^ msg)
  with
  | Error _ as e -> e
  | Ok json ->
    (match parse_pricing_entries_json json with
     | Ok entries ->
       install_pricing_overrides entries;
       Ok ()
     | Error msg -> Error msg)
;;

(** Load pricing overrides from environment variables.

    Checks [OAS_PRICING_FILE] first: if set and non-empty, calls
    {!load_pricing_file} on its value and logs the result via {!Diag}.

    If [OAS_PRICING_FILE] is absent, checks [OAS_PRICING_OVERRIDES] for
    an inline JSON array string and installs it if valid.

    A warning is logged when either source is present but fails to parse,
    so callers fall back to the static table with an observable signal. *)
let pricing_overrides_from_env ?(getenv = Cli_common_env.default_getenv) () =
  match Cli_common_env.get ~getenv "OAS_PRICING_FILE" with
  | Some path ->
    (match load_pricing_file path with
     | Ok () ->
       let n =
         match Atomic.get _overrides with
         | Some (_, es) -> List.length es
         | None -> 0
       in
       Diag.info "pricing" "loaded %d pricing overrides from %s" n path
     | Error msg ->
       Diag.warn "pricing" "failed to load %s: %s; using static table" path msg)
  | None ->
    (match Cli_common_env.get ~getenv "OAS_PRICING_OVERRIDES" with
     | Some raw ->
       (match
          try Ok (Yojson.Safe.from_string raw) with
          | Yojson.Json_error msg -> Error ("JSON parse error: " ^ msg)
        with
        | Error msg ->
          Diag.warn
            "pricing"
            "OAS_PRICING_OVERRIDES parse error: %s; using static table"
            msg
        | Ok json ->
          (match parse_pricing_entries_json json with
           | Ok entries ->
             install_pricing_overrides entries;
             Diag.info
               "pricing"
               "loaded %d pricing overrides from OAS_PRICING_OVERRIDES"
               (List.length entries)
           | Error msg ->
             Diag.warn
               "pricing"
               "OAS_PRICING_OVERRIDES parse error: %s; using static table"
               msg))
     | None -> ())
;;

[@@@coverage off]
(* === Inline tests === *)

let close_enough a b = Float.abs (a -. b) < 1e-9

let pricing_close_enough (a : pricing) (b : pricing) =
  close_enough a.input_per_million b.input_per_million
  && close_enough a.output_per_million b.output_per_million
  && close_enough a.cache_write_multiplier b.cache_write_multiplier
  && close_enough a.cache_read_multiplier b.cache_read_multiplier
;;

(* --- string_contains --- *)

let%test "string_contains: empty needle matches anything" =
  string_contains ~needle:"" "hello"
;;

let%test "string_contains: empty needle matches empty haystack" =
  string_contains ~needle:"" ""
;;

let%test "string_contains: exact match" = string_contains ~needle:"hello" "hello"
let%test "string_contains: prefix match" = string_contains ~needle:"hel" "hello"
let%test "string_contains: suffix match" = string_contains ~needle:"llo" "hello"
let%test "string_contains: middle match" = string_contains ~needle:"ell" "hello"
let%test "string_contains: no match" = not (string_contains ~needle:"xyz" "hello")

let%test "string_contains: needle longer than haystack" =
  not (string_contains ~needle:"hello world" "hello")
;;

let%test "string_contains: case sensitive" = not (string_contains ~needle:"HELLO" "hello")

(* --- pricing_for_model: Anthropic models --- *)

let%test "pricing opus-4-6" =
  let p = pricing_for_model "claude-opus-4-6-20250514" in
  close_enough p.input_per_million 15.0
  && close_enough p.output_per_million 75.0
  && close_enough p.cache_write_multiplier 1.25
  && close_enough p.cache_read_multiplier 0.1
;;

let%test "pricing opus-4-5" =
  let p = pricing_for_model "claude-opus-4-5-20251101" in
  close_enough p.input_per_million 15.0 && close_enough p.output_per_million 75.0
;;

let%test "pricing sonnet-4-6" =
  let p = pricing_for_model "claude-sonnet-4-6-20250514" in
  close_enough p.input_per_million 3.0
  && close_enough p.output_per_million 15.0
  && close_enough p.cache_write_multiplier 1.25
  && close_enough p.cache_read_multiplier 0.1
;;

let%test "pricing sonnet-4 (non-4-6)" =
  let p = pricing_for_model "claude-sonnet-4-20250514" in
  close_enough p.input_per_million 3.0 && close_enough p.output_per_million 15.0
;;

let%test "pricing haiku-4-5" =
  let p = pricing_for_model "claude-haiku-4-5-20251001" in
  close_enough p.input_per_million 0.8 && close_enough p.output_per_million 4.0
;;

let%test "pricing claude-3-7-sonnet" =
  let p = pricing_for_model "claude-3-7-sonnet-20250219" in
  close_enough p.input_per_million 3.0
  && close_enough p.output_per_million 15.0
  && close_enough p.cache_write_multiplier 1.25
;;

(* --- pricing_for_model: Openai models --- *)

let%test "pricing gpt-mini" =
  let p = pricing_for_model "gpt-mini" in
  close_enough p.input_per_million 0.15
  && close_enough p.output_per_million 0.6
  && close_enough p.cache_write_multiplier 1.0
  && close_enough p.cache_read_multiplier 1.0
;;

let%test "pricing gpt-5.5" =
  let p = pricing_for_model "gpt-5.5" in
  close_enough p.input_per_million 5.0
  && close_enough p.output_per_million 30.0
  && close_enough p.cache_write_multiplier 1.0
  && close_enough p.cache_read_multiplier 0.1
;;

let%test "pricing gpt-5.4-mini" =
  let p = pricing_for_model "gpt-5.4-mini" in
  close_enough p.input_per_million 0.75
  && close_enough p.output_per_million 4.5
  && close_enough p.cache_write_multiplier 1.0
  && close_enough p.cache_read_multiplier 0.1
;;

let%test "pricing gpt-5.4" =
  let p = pricing_for_model "gpt-5.4" in
  close_enough p.input_per_million 2.5
  && close_enough p.output_per_million 15.0
  && close_enough p.cache_read_multiplier 0.1
;;

let%test "pricing gpt-5.3-codex" =
  let p = pricing_for_model "gpt-5.3-codex" in
  close_enough p.input_per_million 1.75
  && close_enough p.output_per_million 14.0
  && close_enough p.cache_read_multiplier 0.1
;;

let%test "pricing gpt-5.2" =
  let p = pricing_for_model "gpt-5.2" in
  close_enough p.input_per_million 1.75
  && close_enough p.output_per_million 14.0
  && close_enough p.cache_read_multiplier 0.1
;;

let%test "pricing gpt-5.3-codex-spark remains unknown" =
  pricing_for_model_opt "gpt-5.3-codex-spark" = None
;;

(* The spark sentinel is Delimited_prefix, so future spark variants must not
   fall through to the broader gpt-5.3-codex price. *)
let%test "pricing gpt-5.3-codex-spark-next stays unknown" =
  pricing_for_model_opt "gpt-5.3-codex-spark-next" = None
;;

(* Regression: gpt-4o is a live model and must keep cost annotation after the
   typed Exact "gpt" stopped covering it (Codex P2 on #2127). *)
let%test "pricing gpt-4o restored" =
  match pricing_for_model_opt "gpt-4o" with
  | Some p ->
    close_enough p.input_per_million 2.5 && close_enough p.output_per_million 10.0
  | None -> false
;;

(* gpt-4o-mini must NOT inherit the full gpt-4o rate: the more-specific entry
   keeps it at the cheaper mini price (Codex P2 on #2127). *)
let%test "pricing gpt-4o-mini is the mini rate, not the gpt-4o rate" =
  match pricing_for_model_opt "gpt-4o-mini" with
  | Some p ->
    close_enough p.input_per_million 0.15 && close_enough p.output_per_million 0.6
  | None -> false
;;

(* gpt-4 is a known alias the repo still constructs; the typed Exact "gpt"
   stopped covering it, so the enumerated entry restores its price. *)
let%test "pricing gpt-4 alias restored" =
  match pricing_for_model_opt "gpt-4" with
  | Some p ->
    close_enough p.input_per_million 2.5 && close_enough p.output_per_million 10.0
  | None -> false
;;

(* Base gpt-5 must keep its catalog price (5.0/30.0) in the static fallback;
   the gpt-5.x specifics are longer and still win for their own ids. *)
let%test "pricing gpt-5 base alias covered" =
  match pricing_for_model_opt "gpt-5" with
  | Some p ->
    close_enough p.input_per_million 5.0 && close_enough p.output_per_million 30.0
  | None -> false
;;

let%test "pricing gpt (not mini)" =
  let p = pricing_for_model "gpt" in
  close_enough p.input_per_million 2.5 && close_enough p.output_per_million 10.0
;;

let%test "pricing gpt-4.1" =
  let p = pricing_for_model "gpt-4.1-turbo" in
  close_enough p.input_per_million 2.0 && close_enough p.output_per_million 8.0
;;

let%test "pricing o3-mini" =
  let p = pricing_for_model "o3-mini" in
  close_enough p.input_per_million 1.1 && close_enough p.output_per_million 4.4
;;

(* --- pricing_for_model: Gemini 3-계 preview (2026-04-16) --- *)

let%test "pricing gemini-3-flash-preview" =
  let p = pricing_for_model "gemini-3-flash-preview" in
  close_enough p.input_per_million 0.50 && close_enough p.output_per_million 3.0
;;

let%test "pricing gemini-3.1-pro-preview" =
  let p = pricing_for_model "gemini-3.1-pro-preview" in
  close_enough p.input_per_million 2.0 && close_enough p.output_per_million 12.0
;;

let%test "pricing gemini-3.1-pro (bare id)" =
  let p = pricing_for_model "gemini-3.1-pro" in
  close_enough p.input_per_million 2.0 && close_enough p.output_per_million 12.0
;;

let%test "pricing gemini-3.1-flash-lite-preview" =
  let p = pricing_for_model "gemini-3.1-flash-lite-preview" in
  close_enough p.input_per_million 0.25 && close_enough p.output_per_million 1.5
;;

(* --- pricing_for_model: Glm (Z.ai) --- *)

let%test "pricing glm-5.1" =
  let p = pricing_for_model "glm-5.1" in
  close_enough p.input_per_million 1.4
  && close_enough p.output_per_million 4.4
  && close_enough p.cache_write_multiplier 1.0
  && close_enough p.cache_read_multiplier (0.26 /. 1.4)
;;

let%test "pricing glm-5-turbo" =
  let p = pricing_for_model "glm-5-turbo" in
  close_enough p.input_per_million 1.2
  && close_enough p.output_per_million 4.0
  && close_enough p.cache_read_multiplier 0.2
;;

let%test "pricing glm-5 (generic)" =
  let p = pricing_for_model "glm-5" in
  close_enough p.input_per_million 1.0 && close_enough p.output_per_million 3.2
;;

let%test "pricing glm-4.7-flashx (paid)" =
  let p = pricing_for_model "glm-4.7-flashx" in
  close_enough p.input_per_million 0.07 && close_enough p.output_per_million 0.4
;;

let%test "pricing glm-4.7-flash (free)" =
  let p = pricing_for_model "glm-4.7-flash" in
  close_enough p.input_per_million 0.0 && close_enough p.output_per_million 0.0
;;

let%test "pricing glm-4.5-x" =
  let p = pricing_for_model "glm-4.5-x" in
  close_enough p.input_per_million 2.2 && close_enough p.output_per_million 8.9
;;

let%test "pricing glm-4.5-airx" =
  let p = pricing_for_model "glm-4.5-airx" in
  close_enough p.input_per_million 1.1 && close_enough p.output_per_million 4.5
;;

let%test "pricing glm-4.5-air" =
  let p = pricing_for_model "glm-4.5-air" in
  close_enough p.input_per_million 0.2 && close_enough p.output_per_million 1.1
;;

let%test "pricing glm-4.5-flash (free)" =
  let p = pricing_for_model "glm-4.5-flash" in
  close_enough p.input_per_million 0.0 && close_enough p.output_per_million 0.0
;;

let%test "pricing glm-4.7 (generic)" =
  let p = pricing_for_model "glm-4.7" in
  close_enough p.input_per_million 0.6 && close_enough p.output_per_million 2.2
;;

let%test "pricing glm-4.5 (generic)" =
  let p = pricing_for_model "glm-4.5" in
  close_enough p.input_per_million 0.6 && close_enough p.output_per_million 2.2
;;

let%test "pricing glm-coding-plan:glm-5-turbo (prefixed variant)" =
  let p = pricing_for_model "glm-coding-plan:glm-5-turbo" in
  close_enough p.input_per_million 1.2 && close_enough p.output_per_million 4.0
;;

let%test "pricing glm-coding-plan:glm-5.1 (prefixed variant)" =
  let p = pricing_for_model "glm-coding-plan:glm-5.1" in
  close_enough p.input_per_million 1.4 && close_enough p.output_per_million 4.4
;;

let%test "pricing_for_model_opt: glm unknown returns None" =
  match pricing_for_model_opt "glm-future-99" with
  | None -> true
  | Some _ -> false
;;

(* --- pricing_for_model: claude_code alias fallback --- *)

let%test "pricing claude_code:auto falls back to sonnet-4-6 rates" =
  let p = pricing_for_model "claude_code:auto" in
  close_enough p.input_per_million 3.0 && close_enough p.output_per_million 15.0
;;

let%test "pricing claude_code (bare alias)" =
  let p = pricing_for_model "claude_code" in
  close_enough p.input_per_million 3.0 && close_enough p.output_per_million 15.0
;;

let%test "pricing cc: short alias falls back to sonnet-4-6 rates" =
  let p = pricing_for_model "cc:default" in
  close_enough p.input_per_million 3.0 && close_enough p.output_per_million 15.0
;;

let%test "pricing_for_model_opt returns Some for gemini-3-flash-preview" =
  match pricing_for_model_opt "gemini-3-flash-preview" with
  | Some p -> p.input_per_million > 0.0
  | None -> false
;;

(* --- pricing_for_model: local/free models --- *)

let%test "pricing ollama is free" =
  let p = pricing_for_model "ollama/llama-3" in
  close_enough p.input_per_million 0.0 && close_enough p.output_per_million 0.0
;;

let%test "pricing dashscope is free" =
  let p = pricing_for_model "dashscope-3.5-35b" in
  close_enough p.input_per_million 0.0
;;

let%test "pricing llama is free" =
  let p = pricing_for_model "llama-3.1-70b" in
  close_enough p.input_per_million 0.0
;;

let%test "pricing_for_model: unknown model falls back to zero" =
  let p = pricing_for_model "some-random-model" in
  close_enough p.input_per_million 0.0 && close_enough p.output_per_million 0.0
;;

(* --- pricing_for_model_opt: distinguishes unknown from free --- *)

let%test "pricing_for_model_opt: known cloud model returns Some" =
  match pricing_for_model_opt "claude-opus-4-6" with
  | Some p -> p.input_per_million > 0.0
  | None -> false
;;

let%test "pricing_for_model_opt: known local model returns Some with zero pricing" =
  match pricing_for_model_opt "ollama/llama-3" with
  | Some p -> close_enough p.input_per_million 0.0
  | None -> false
;;

let%test "pricing_for_model_opt: dashscope returns Some" =
  match pricing_for_model_opt "dashscope-3.5-35b" with
  | Some _ -> true
  | None -> false
;;

let%test "pricing_for_model_opt: unknown model returns None" =
  match pricing_for_model_opt "some-random-model" with
  | Some _ -> false
  | None -> true
;;

let%test "pricing_for_model_opt: cloud-style unknown returns None" =
  match pricing_for_model_opt "future-cloud-provider/fancy-model-v9" with
  | Some _ -> false
  | None -> true
;;

let with_empty_catalog f =
  let original = Model_catalog.global () in
  Model_catalog.set_global [];
  Fun.protect
    ~finally:(fun () ->
      match original with
      | Some c -> Model_catalog.set_global c
      | None -> Model_catalog.clear_global ())
    f
;;

(* Install a synthetic catalog from inline TOML for the duration of [f], then
   restore the original. Goes through [Model_catalog.load_file] so the test
   exercises the real parse path and stays robust to new optional fields. *)
let with_catalog_toml content f =
  let path = Filename.temp_file "oas_pricing_catalog" ".toml" in
  let original = Model_catalog.global () in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove path with
       | Sys_error _ -> ());
      match original with
      | Some c -> Model_catalog.set_global c
      | None -> Model_catalog.clear_global ())
    (fun () ->
       let oc = open_out path in
       output_string oc content;
       close_out oc;
       match Model_catalog.load_file path with
       | Ok catalog ->
         Model_catalog.set_global catalog;
         f ()
       | Error e -> failwith ("test catalog load failed: " ^ e))
;;

(* A catalog entry that applies to the id but deliberately omits pricing marks
   the model unpriced; the static fallback must not override that intent with a
   default rate (static would otherwise price gpt-4o at 2.5/10.0). Codex P2 on
   #2127. *)
let%test "pricing_for_model_opt: catalog entry with omitted price stays unpriced" =
  with_catalog_toml "[[models]]\nid_prefix = \"gpt-4o\"\n" (fun () ->
    pricing_for_model_opt "gpt-4o" = None)
;;

(* A deliberate "unpriced" only suppresses the static fallback for the id the
   caller actually asked about. A provider-stripped candidate that merely hits
   an unpriced catalog capability entry must not suppress the original id's
   static/free classification: "dashscope/qwen3-32b" stays free even though the
   stripped "qwen3-32b" matches the unpriced "qwen3" entry. Codex P2 on #2127. *)
let%test
    "pricing_for_model_opt: provider-prefixed free id survives unpriced stripped catalog \
     match"
  =
  with_catalog_toml "[[models]]\nid_prefix = \"qwen3\"\n" (fun () ->
    match pricing_for_model_opt "dashscope/qwen3-32b" with
    | Some p ->
      close_enough p.input_per_million 0.0 && close_enough p.output_per_million 0.0
    | None -> false)
;;

(* A catalog id_prefix that ends in a delimiter (here a "/"-terminated provider
   namespace) is a raw prefix: "myorg/" must price "myorg/model-a" rather than
   requiring an extra delimiter after the slash. Codex P2 on #2127. *)
let%test "pricing_for_model_opt: slash-terminated catalog prefix prices namespaced ids" =
  with_catalog_toml
    "[[models]]\n\
     id_prefix = \"myorg/\"\n\
     input_per_million = 1.0\n\
     output_per_million = 2.0\n"
    (fun () ->
       match pricing_for_model_opt "myorg/model-a" with
       | Some p ->
         close_enough p.input_per_million 1.0 && close_enough p.output_per_million 2.0
       | None -> false)
;;

(* A genuine catalog miss (no applicable entry) still consults the static
   fallback rather than reporting the model unpriced. *)
let%test "pricing_for_model_opt: catalog miss still consults static fallback" =
  with_catalog_toml
    "[[models]]\n\
     id_prefix = \"unrelated-vendor-x\"\n\
     input_per_million = 1.0\n\
     output_per_million = 2.0\n"
    (fun () ->
       match pricing_for_model_opt "gpt-4o" with
       | Some p ->
         close_enough p.input_per_million 2.5 && close_enough p.output_per_million 10.0
       | None -> false)
;;

let%test "pricing_for_model_opt: built-in fallback when catalog is absent" =
  with_empty_catalog (fun () ->
    match pricing_for_model_opt "claude-sonnet-4-6" with
    | Some p ->
      close_enough p.input_per_million 3.0 && close_enough p.output_per_million 15.0
    | None -> false)
;;

(* Catalog-absent fallback: dated gpt-4o ids price like gpt-4o (delimiter
   prefix), while gpt-4o-mini stays at its own cheaper rate (longer entry wins
   the length-sorted lookup). Codex P2 on #2127. *)
let%test "pricing_for_model_opt: dated gpt-4o prices without catalog, mini stays mini" =
  with_empty_catalog (fun () ->
    match
      pricing_for_model_opt "gpt-4o-2024-08-06", pricing_for_model_opt "gpt-4o-mini"
    with
    | Some dated, Some mini ->
      close_enough dated.input_per_million 2.5
      && close_enough dated.output_per_million 10.0
      && close_enough mini.input_per_million 0.15
      && close_enough mini.output_per_million 0.6
    | _ -> false)
;;

let%test "pricing_for_model_opt: provider-prefixed id falls back to built-in table" =
  with_empty_catalog (fun () ->
    match pricing_for_model_opt "anthropic/claude-sonnet-4-6" with
    | Some p -> close_enough p.input_per_million 3.0
    | None -> false)
;;

let%test "pricing_for_model_opt: explicit unknown remains unknown without catalog" =
  with_empty_catalog (fun () -> pricing_for_model_opt "gpt-5.3-codex-spark" = None)
;;

let%test "pricing_for_model_opt: future gpt family remains unknown" =
  pricing_for_model_opt "gpt-6-turbo" = None
;;

let%test "pricing_for_model_opt: broad gpt fallback does not price future family" =
  with_empty_catalog (fun () -> pricing_for_model_opt "gpt-6-turbo" = None)
;;

let%test "pricing_for_model_opt: substring free aliases do not match paid-looking ids" =
  with_empty_catalog (fun () ->
    pricing_for_model_opt "future-ollama-paid" = None
    && pricing_for_model_opt "paid-dashscope-compatible" = None
    && pricing_for_model_opt "paid-nous-compatible" = None)
;;

let%test "pricing_for_model_opt: anchored free aliases still work without catalog" =
  with_empty_catalog (fun () ->
    match
      pricing_for_model_opt "ollama/llama-3", pricing_for_model_opt "dashscope-3.5-35b"
    with
    | Some ollama, Some dashscope ->
      close_enough ollama.input_per_million 0.0
      && close_enough ollama.output_per_million 0.0
      && close_enough dashscope.input_per_million 0.0
      && close_enough dashscope.output_per_million 0.0
    | _ -> false)
;;

(* DashScope route ids use "_" as a separator (the in-tree catalog "dashscope_3"
   entry). Without the catalog, the static free-alias check must still classify
   them as zero-priced rather than unknown. Codex P2 on #2127. *)
let%test
    "pricing_for_model_opt: underscore-separated dashscope id is free without catalog"
  =
  with_empty_catalog (fun () ->
    match pricing_for_model_opt "dashscope_3", pricing_for_model_opt "dashscope_3.5" with
    | Some a, Some b ->
      close_enough a.input_per_million 0.0
      && close_enough a.output_per_million 0.0
      && close_enough b.input_per_million 0.0
      && close_enough b.output_per_million 0.0
    | _ -> false)
;;

let%test "built-in pricing fallback matches catalog pricing overlaps" =
  match Model_catalog.global () with
  | None -> true
  | Some catalog ->
    List.for_all
      (fun entry ->
         match entry.pricing with
         | None -> true
         | Some expected ->
           (match catalog_pricing_opt catalog entry.key with
            | Some actual -> pricing_close_enough actual expected
            | None -> false))
      static_pricing_entries
;;

(* --- pricing_for_model: case insensitivity --- *)

let%test "pricing case insensitive" =
  let p = pricing_for_model "Claude-Opus-4-6" in
  close_enough p.input_per_million 15.0
;;

let%test "pricing whitespace trimmed" =
  let p = pricing_for_model "  claude-sonnet-4-6  " in
  close_enough p.input_per_million 3.0
;;

(* --- estimate_cost --- *)

let%test "estimate_cost: zero tokens is zero" =
  let p = pricing_for_model "claude-opus-4-6" in
  close_enough (estimate_cost ~pricing:p ~input_tokens:0 ~output_tokens:0 ()) 0.0
;;

let%test "estimate_cost: 1M input tokens opus" =
  let p = pricing_for_model "claude-opus-4-6" in
  let cost = estimate_cost ~pricing:p ~input_tokens:1_000_000 ~output_tokens:0 () in
  close_enough cost 15.0
;;

let%test "estimate_cost: 1M output tokens opus" =
  let p = pricing_for_model "claude-opus-4-6" in
  let cost = estimate_cost ~pricing:p ~input_tokens:0 ~output_tokens:1_000_000 () in
  close_enough cost 75.0
;;

let%test "estimate_cost: mixed input and output" =
  let p = pricing_for_model "claude-sonnet-4-6" in
  let cost = estimate_cost ~pricing:p ~input_tokens:1000 ~output_tokens:500 () in
  (* 1000 * 3.0/1M + 500 * 15.0/1M = 0.003 + 0.0075 = 0.0105 *)
  close_enough cost 0.0105
;;

let%test "estimate_cost: with cache write tokens" =
  let p = pricing_for_model "claude-opus-4-6" in
  (* 1000 input, 500 cache write, 0 cache read, 0 output *)
  (* regular_input = 1000 - 500 - 0 = 500 *)
  (* input_cost = 500 * 15/1M = 0.0075 *)
  (* cache_write = 500 * 15/1M * 1.25 = 0.009375 *)
  let cost =
    estimate_cost
      ~pricing:p
      ~input_tokens:1000
      ~output_tokens:0
      ~cache_creation_input_tokens:500
      ()
  in
  close_enough cost (0.0075 +. 0.009375)
;;

let%test "estimate_cost: with cache read tokens" =
  let p = pricing_for_model "claude-opus-4-6" in
  (* 1000 input, 0 cache write, 200 cache read, 0 output *)
  (* regular_input = 1000 - 0 - 200 = 800 *)
  (* input_cost = 800 * 15/1M = 0.012 *)
  (* cache_read = 200 * 15/1M * 0.1 = 0.0003 *)
  let cost =
    estimate_cost
      ~pricing:p
      ~input_tokens:1000
      ~output_tokens:0
      ~cache_read_input_tokens:200
      ()
  in
  close_enough cost (0.012 +. 0.0003)
;;

let%test "estimate_cost: regular_input clamped to zero when cache exceeds total" =
  let p = pricing_for_model "claude-sonnet-4-6" in
  (* input_tokens=100 but cache_creation=200: regular = max 0 (100-200) = 0 *)
  let cost =
    estimate_cost
      ~pricing:p
      ~input_tokens:100
      ~output_tokens:0
      ~cache_creation_input_tokens:200
      ()
  in
  (* only cache_write cost: 200 * 3/1M * 1.25 = 0.00075 *)
  close_enough cost 0.00075
;;

let%test "estimate_cost: free model is always zero" =
  let p = pricing_for_model "dashscope-3.5" in
  let cost =
    estimate_cost
      ~pricing:p
      ~input_tokens:1_000_000
      ~output_tokens:1_000_000
      ~cache_creation_input_tokens:500_000
      ~cache_read_input_tokens:500_000
      ()
  in
  close_enough cost 0.0
;;

let%test "annotate_usage_cost fills missing cost for known model" =
  let usage : Types.api_usage =
    { input_tokens = 1_000
    ; output_tokens = 500
    ; cache_creation_input_tokens = 0
    ; cache_read_input_tokens = 0
    ; cost_usd = None
    }
  in
  match annotate_usage_cost ~model_id:"claude-sonnet-4-6" usage with
  | { cost_usd = Some cost; _ } -> cost > 0.0
  | _ -> false
;;

let%test "annotate_usage_cost leaves cost_usd None for unknown model" =
  let usage : Types.api_usage =
    { input_tokens = 1_000
    ; output_tokens = 500
    ; cache_creation_input_tokens = 0
    ; cache_read_input_tokens = 0
    ; cost_usd = None
    }
  in
  match annotate_usage_cost ~model_id:"totally-unknown-cloud-model" usage with
  | { cost_usd = None; _ } -> true
  | _ -> false
;;

let%test "annotate_usage_cost fills zero cost for known free model" =
  let usage : Types.api_usage =
    { input_tokens = 1_000
    ; output_tokens = 500
    ; cache_creation_input_tokens = 0
    ; cache_read_input_tokens = 0
    ; cost_usd = None
    }
  in
  match annotate_usage_cost ~model_id:"dashscope-3.5-35b" usage with
  | { cost_usd = Some cost; _ } -> close_enough cost 0.0
  | _ -> false
;;

let%test "annotate_response_cost preserves measured cost" =
  let response : Types.api_response =
    { id = "resp-1"
    ; model = "claude-sonnet-4-6"
    ; stop_reason = Types.EndTurn
    ; content = [ Types.Text "ok" ]
    ; usage =
        Some
          { input_tokens = 100
          ; output_tokens = 20
          ; cache_creation_input_tokens = 0
          ; cache_read_input_tokens = 0
          ; cost_usd = Some 0.1234
          }
    ; telemetry = None
    }
  in
  match annotate_response_cost response with
  | { usage = Some { cost_usd = Some cost; _ }; _ } -> close_enough cost 0.1234
  | _ -> false
;;

(* ── Dynamic override tests ─────────────────────────────────── *)

let%test "install_pricing_overrides: override takes priority over static table" =
  let entry =
    { pattern = "my-custom-model"
    ; input_per_million = 42.0
    ; output_per_million = 84.0
    ; cache_write_multiplier = 1.0
    ; cache_read_multiplier = 1.0
    }
  in
  install_pricing_overrides [ entry ];
  let result =
    match pricing_for_model_opt "my-custom-model-v1" with
    | Some p -> close_enough p.input_per_million 42.0
    | None -> false
  in
  clear_pricing_overrides ();
  result
;;

let%test "clear_pricing_overrides: restores static table" =
  let entry =
    { pattern = "claude-opus-4-6"
    ; input_per_million = 999.0
    ; output_per_million = 999.0
    ; cache_write_multiplier = 1.0
    ; cache_read_multiplier = 1.0
    }
  in
  install_pricing_overrides [ entry ];
  clear_pricing_overrides ();
  let result =
    match pricing_for_model_opt "claude-opus-4-6" with
    | Some p -> close_enough p.input_per_million 15.0
    | None -> false
  in
  result
;;

let%test "install_pricing_overrides: overrides shadow static table entry" =
  let entry =
    { pattern = "claude-opus-4-6"
    ; input_per_million = 20.0
    ; output_per_million = 100.0
    ; cache_write_multiplier = 1.3
    ; cache_read_multiplier = 0.05
    }
  in
  install_pricing_overrides [ entry ];
  let result =
    match pricing_for_model_opt "claude-opus-4-6" with
    | Some p ->
      close_enough p.input_per_million 20.0 && close_enough p.output_per_million 100.0
    | None -> false
  in
  clear_pricing_overrides ();
  result
;;

let%test "install_pricing_overrides: unknown model still falls through to static table" =
  let entry =
    { pattern = "other-model"
    ; input_per_million = 1.0
    ; output_per_million = 2.0
    ; cache_write_multiplier = 1.0
    ; cache_read_multiplier = 1.0
    }
  in
  install_pricing_overrides [ entry ];
  let result =
    match pricing_for_model_opt "claude-opus-4-6" with
    | Some p -> close_enough p.input_per_million 15.0
    | None -> false
  in
  clear_pricing_overrides ();
  result
;;

let%test "parse_pricing_entries_json: valid array" =
  let json =
    Yojson.Safe.from_string
      {|[{"pattern":"test-model","input_per_million":1.5,"output_per_million":6.0}]|}
  in
  match parse_pricing_entries_json json with
  | Ok [ e ] ->
    e.pattern = "test-model"
    && close_enough e.input_per_million 1.5
    && close_enough e.output_per_million 6.0
    && close_enough e.cache_write_multiplier 1.0
    && close_enough e.cache_read_multiplier 1.0
  | _ -> false
;;

let%test "parse_pricing_entries_json: optional cache multipliers default to 1.0" =
  let json =
    Yojson.Safe.from_string
      {|[{"pattern":"m","input_per_million":0.5,"output_per_million":2.0,"cache_write_multiplier":1.25,"cache_read_multiplier":0.1}]|}
  in
  match parse_pricing_entries_json json with
  | Ok [ e ] ->
    close_enough e.cache_write_multiplier 1.25 && close_enough e.cache_read_multiplier 0.1
  | _ -> false
;;

let%test "parse_pricing_entries_json: not an array returns Error" =
  let json = Yojson.Safe.from_string {|{"pattern":"m","input_per_million":1.0}|} in
  match parse_pricing_entries_json json with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "parse_pricing_entries_json: empty pattern returns Error" =
  let json =
    Yojson.Safe.from_string
      {|[{"pattern":"","input_per_million":1.0,"output_per_million":2.0}]|}
  in
  match parse_pricing_entries_json json with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "load_pricing_file: non-existent file returns Error" =
  match load_pricing_file "/tmp/oas_pricing_does_not_exist_xyz.json" with
  | Error _ -> true
  | Ok () -> false
;;

let%test "pricing_overrides_from_env: OAS_PRICING_OVERRIDES inline JSON via env boundary" =
  let json_str =
    {|[{"pattern":"env-test-model","input_per_million":7.0,"output_per_million":21.0}]|}
  in
  let getenv = function
    | "OAS_PRICING_OVERRIDES" -> Some ("  " ^ json_str ^ "  ")
    | _ -> None
  in
  pricing_overrides_from_env ~getenv ();
  let result =
    match pricing_for_model_opt "env-test-model-v2" with
    | Some p -> close_enough p.input_per_million 7.0
    | None -> false
  in
  clear_pricing_overrides ();
  result
;;
