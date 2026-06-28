(** Tool selector: 2-stage tool routing for large tool catalogs.

    Narrows the tool set before the LLM composes arguments, improving
    selection accuracy from ~42% to 83-100% for catalogs with 20+ tools.

    @since 0.100.0 *)

type strategy =
  | All
  | TopK_bm25 of
      { k : int
      ; always_include : string list
      ; confidence_threshold : float option
      ; fallback_tools : string list
      }
  | TopK_llm of
      { k : int
      ; bm25_prefilter_n : int
      ; always_include : string list
      ; confidence_threshold : float
      ; rerank_fn : context:string -> candidates:(string * string) list -> string list
      }
  | Categorical of
      { groups : (string * string list) list
      ; classifier :
          [ `Bm25
          | `Llm of context:string -> candidates:(string * string) list -> string list
          ]
      ; always_include : string list
      }

exception Unsupported_configuration of string

(* ── Helpers ─────────────────────────────────────────────── *)

(** Build a set of selected names, deduplicating.
    [always_include] names come first, then [top_names]. *)
let merge_names ~always_include ~top_names =
  let seen = Hashtbl.create 16 in
  let result = ref [] in
  let add name =
    if not (Hashtbl.mem seen name)
    then (
      Hashtbl.replace seen name true;
      result := name :: !result)
  in
  List.iter add always_include;
  List.iter add top_names;
  List.rev !result
;;

(** Filter [tools] to only those whose [schema.name] is in [names].
    Preserves original order of [tools] and keeps the first descriptor for
    duplicate names, matching the agent execution index. *)
let filter_by_names names tools =
  let set = Hashtbl.create (List.length names) in
  List.iter (fun n -> Hashtbl.replace set n true) names;
  let emitted = Hashtbl.create (List.length names) in
  List.filter
    (fun (t : Tool.t) ->
       let name = t.schema.name in
       if Hashtbl.mem set name && not (Hashtbl.mem emitted name)
       then (
         Hashtbl.replace emitted name true;
         true)
       else false)
    tools
;;

let build_tool_name_index tools =
  let index = Hashtbl.create (max 16 (List.length tools * 2)) in
  List.iter
    (fun (tool : Tool.t) ->
       if not (Hashtbl.mem index tool.schema.name)
       then Hashtbl.add index tool.schema.name tool)
    tools;
  index
;;

(* ── Strategy implementations ────────────────────────────── *)

let select_all tools = tools

let select_llm_core
      ~k
      ~bm25_prefilter_n
      ~always_include
      ~confidence_threshold
      ~rerank_fn
      ~context
      ~index
      ~tools
  =
  let retrieved = Tool_index.retrieve index context in
  let top_score =
    match retrieved with
    | (_, s) :: _ -> s
    | [] -> 0.0
  in
  (* Confidence gate: low score -> skip LLM, use BM25 top-k directly *)
  if top_score < confidence_threshold
  then (
    let top_names = List.filteri (fun i _ -> i < k) retrieved |> List.map fst in
    let selected = merge_names ~always_include ~top_names in
    filter_by_names selected tools)
  else (
    (* Stage 1: BM25 pre-filter *)
    let bm25_top = List.filteri (fun i _ -> i < bm25_prefilter_n) retrieved in
    let tool_by_name = build_tool_name_index tools in
    (* Build (name, description) candidates for reranker *)
    let candidates =
      List.filter_map
        (fun (name, _score) ->
           match Hashtbl.find_opt tool_by_name name with
           | Some t -> Some (name, t.schema.description)
           | None -> None)
        bm25_top
    in
    (* Stage 2: LLM rerank. Provider/classifier failure is fail-closed:
       return no LLM-selected names and keep only [always_include]. *)
    let llm_selected =
      try rerank_fn ~context ~candidates with
      | (Out_of_memory | Stack_overflow | Sys.Break) as exn -> raise exn
      | _exn -> []
    in
    (* Validate: only keep names that exist in candidates *)
    let candidate_set = Hashtbl.create (List.length candidates) in
    List.iter (fun (n, _) -> Hashtbl.replace candidate_set n true) candidates;
    let valid_names = List.filter (fun n -> Hashtbl.mem candidate_set n) llm_selected in
    let top_names = List.filteri (fun i _ -> i < k) valid_names in
    let selected = merge_names ~always_include ~top_names in
    filter_by_names selected tools)
;;

let select_bm25_core
      ~k
      ~always_include
      ~confidence_threshold
      ~fallback_tools
      ~context
      ~index
      ~tools
  =
  let retrieved = Tool_index.retrieve index context in
  let top_score =
    match retrieved with
    | (_, s) :: _ -> s
    | [] -> 0.0
  in
  let use_fallback =
    match confidence_threshold with
    | Some t -> top_score < t
    | None -> false
  in
  let top_names = List.filteri (fun i _ -> i < k) retrieved |> List.map fst in
  let extra = if use_fallback then fallback_tools else [] in
  let selected_names = merge_names ~always_include ~top_names:(top_names @ extra) in
  filter_by_names selected_names tools
;;

let group_candidates ~tools groups =
  let tool_by_name = build_tool_name_index tools in
  List.map
    (fun (group_name, tool_names) ->
       let description =
         tool_names
         |> List.map (fun name ->
           match Hashtbl.find_opt tool_by_name name with
           | Some (tool : Tool.t) -> name ^ ": " ^ tool.schema.description
           | None -> name)
         |> String.concat "\n"
       in
       group_name, description)
    groups
;;

let select_categorical_bm25 ~groups ~always_include ~context ~tools =
  let candidates = group_candidates ~tools groups in
  let group_entries =
    List.map
      (fun (group_name, description) ->
         { Tool_index.name = group_name; description; group = None; aliases = [] })
      candidates
  in
  let group_index = Tool_index.build group_entries in
  Tool_index.retrieve_names group_index context
  |> fun group_names ->
  let group_tool_names =
    List.concat_map
      (fun gname ->
         match List.assoc_opt gname groups with
         | Some names -> names
         | None -> [])
      group_names
  in
  let selected_names = merge_names ~always_include ~top_names:group_tool_names in
  filter_by_names selected_names tools
;;

let select_categorical_llm ~groups ~classifier ~always_include ~context ~tools =
  let candidates = group_candidates ~tools groups in
  let candidate_set = Hashtbl.create (List.length candidates) in
  List.iter (fun (name, _) -> Hashtbl.replace candidate_set name true) candidates;
  let group_names =
    try classifier ~context ~candidates with
    | (Out_of_memory | Stack_overflow | Sys.Break) as exn -> raise exn
    | _exn -> []
  in
  let valid_group_names =
    List.filter (fun name -> Hashtbl.mem candidate_set name) group_names
  in
  let group_tool_names =
    List.concat_map
      (fun gname ->
         match List.assoc_opt gname groups with
         | Some names -> names
         | None -> [])
      valid_group_names
  in
  let selected_names = merge_names ~always_include ~top_names:group_tool_names in
  filter_by_names selected_names tools
;;

(* ── Convenience wrappers (build index internally) ───────── *)

let select_llm
      ~k
      ~bm25_prefilter_n
      ~always_include
      ~confidence_threshold
      ~rerank_fn
      ~context
      ~tools
  =
  if tools = []
  then []
  else (
    let index = Tool_index.of_tools tools in
    select_llm_core
      ~k
      ~bm25_prefilter_n
      ~always_include
      ~confidence_threshold
      ~rerank_fn
      ~context
      ~index
      ~tools)
;;

let select_bm25 ~k ~always_include ~confidence_threshold ~fallback_tools ~context ~tools =
  if tools = []
  then []
  else (
    let index = Tool_index.of_tools tools in
    select_bm25_core
      ~k
      ~always_include
      ~confidence_threshold
      ~fallback_tools
      ~context
      ~index
      ~tools)
;;

(* ── Public API ──────────────────────────────────────────── *)

let select ~strategy ~context ~tools =
  match strategy with
  | All -> select_all tools
  | TopK_bm25 { k; always_include; confidence_threshold; fallback_tools } ->
    select_bm25 ~k ~always_include ~confidence_threshold ~fallback_tools ~context ~tools
  | TopK_llm { k; bm25_prefilter_n; always_include; confidence_threshold; rerank_fn } ->
    select_llm
      ~k
      ~bm25_prefilter_n
      ~always_include
      ~confidence_threshold
      ~rerank_fn
      ~context
      ~tools
  | Categorical { groups; classifier = `Bm25; always_include } ->
    if tools = []
    then []
    else select_categorical_bm25 ~groups ~always_include ~context ~tools
  | Categorical { groups; classifier = `Llm classifier; always_include } ->
    if tools = []
    then []
    else select_categorical_llm ~groups ~classifier ~always_include ~context ~tools
;;

let select_with_index ~strategy ~index ~context ~tools =
  match strategy with
  | All -> select_all tools
  | TopK_bm25 { k; always_include; confidence_threshold; fallback_tools } ->
    if tools = []
    then []
    else
      select_bm25_core
        ~k
        ~always_include
        ~confidence_threshold
        ~fallback_tools
        ~context
        ~index
        ~tools
  | TopK_llm { k; bm25_prefilter_n; always_include; confidence_threshold; rerank_fn } ->
    if tools = []
    then []
    else
      select_llm_core
        ~k
        ~bm25_prefilter_n
        ~always_include
        ~confidence_threshold
        ~rerank_fn
        ~context
        ~index
        ~tools
  | Categorical _ ->
    (* Categorical builds its own group index; the tool index is not useful.
       Fall through to the standard select path. *)
    select ~strategy ~context ~tools
;;

let select_names ~strategy ~context ~tools =
  List.map (fun (t : Tool.t) -> t.schema.name) (select ~strategy ~context ~tools)
;;

let select_names_with_index ~strategy ~index ~context ~tools =
  List.map
    (fun (t : Tool.t) -> t.schema.name)
    (select_with_index ~strategy ~index ~context ~tools)
;;

let auto ~tools =
  if List.length tools <= 15
  then All
  else
    TopK_bm25
      { k = 5; always_include = []; confidence_threshold = None; fallback_tools = [] }
;;

(* ── Default rerank function ───────────────────────────── *)

let default_rerank_fn ~sw ~net ~provider ~k () =
  fun ~context ~candidates ->
  let candidate_names = Hashtbl.create (List.length candidates) in
  List.iter (fun (name, _) -> Hashtbl.replace candidate_names name true) candidates;
  let tool_list =
    List.mapi
      (fun i (name, desc) -> Printf.sprintf "%d. %s: %s" (i + 1) name desc)
      candidates
    |> String.concat "\n"
  in
  let prompt =
    Printf.sprintf
      "Given the user query below, select the %d most relevant tools from the list. \
       Return ONLY a JSON array of tool-name strings, in order of relevance. No \
       markdown, no prose, no object wrapper.\n\n\
       Query: %s\n\n\
       Available tools:\n\
       %s"
      k
      context
      tool_list
  in
  let messages =
    [ { Types.role = Types.User
      ; content = [ Types.Text prompt ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let parse_json_name_array text =
    try
      match Yojson.Safe.from_string text with
      | `List items ->
        items
        |> List.filter_map (function
          | `String name when Hashtbl.mem candidate_names name -> Some name
          | `String _unknown_name -> None
          | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `Assoc _ | `List _ -> None)
      | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Assoc _ -> []
    with
    | Yojson.Json_error _ -> []
  in
  (* Single-provider rerank. Deterministic sampling for reproducibility. The
     response contract is structured JSON; malformed/provider-error responses
     select nothing, leaving only caller-specified [always_include] upstream. *)
  let provider_cfg =
    { provider with
      Llm_provider.Provider_config.temperature = Some 0.0
    ; max_tokens = Some 200
    }
  in
  match
    Llm_provider.Complete.complete ~sw ~net ~config:provider_cfg ~messages ~tools:[] ()
  with
  | Ok response ->
    let text = Types.text_of_response response in
    parse_json_name_array text
  | Error _ -> []
;;
