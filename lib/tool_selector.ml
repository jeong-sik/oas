(** Tool selector: 2-stage tool routing for large tool catalogs.

    Narrows the tool set before the LLM composes arguments, improving
    selection accuracy from ~42% to 83-100% for catalogs with 20+ tools.

    @since 0.100.0 *)

type strategy =
  | All
  | TopK_bm25 of {
      k: int;
      always_include: string list;
      confidence_threshold: float option;
      fallback_tools: string list;
    }
  | TopK_llm of {
      k: int;
      bm25_prefilter_n: int;
      always_include: string list;
      confidence_threshold: float;
      rerank_fn:
        (context:string ->
         candidates:(string * string) list ->
         string list);
    }
  | Categorical of {
      groups: (string * string list) list;
      classifier: [ `Bm25 | `Llm ];
      always_include: string list;
    }

(* ── Helpers ─────────────────────────────────────────────── *)

(** Build a set of selected names, deduplicating.
    [always_include] names come first, then [top_names]. *)
let merge_names ~always_include ~top_names =
  let seen = Hashtbl.create 16 in
  let result = ref [] in
  let add name =
    if not (Hashtbl.mem seen name) then begin
      Hashtbl.replace seen name true;
      result := name :: !result
    end
  in
  List.iter add always_include;
  List.iter add top_names;
  List.rev !result

(** Filter [tools] to only those whose [schema.name] is in [names].
    Preserves original order of [tools]. *)
let filter_by_names names tools =
  let set = Hashtbl.create (List.length names) in
  List.iter (fun n -> Hashtbl.replace set n true) names;
  List.filter (fun (t : Tool.t) ->
    Hashtbl.mem set t.schema.name
  ) tools

(* ── Strategy implementations ────────────────────────────── *)

let select_all tools = tools

let select_llm ~k ~bm25_prefilter_n ~always_include ~confidence_threshold
    ~rerank_fn ~context ~tools =
  if tools = [] then []
  else
    let index = Tool_index.of_tools tools in
    let retrieved = Tool_index.retrieve index context in
    let top_score = match retrieved with
      | (_, s) :: _ -> s
      | [] -> 0.0
    in
    (* Confidence gate: low score -> skip LLM, use BM25 top-k directly *)
    if top_score < confidence_threshold then
      let top_names =
        List.filteri (fun i _ -> i < k) retrieved |> List.map fst
      in
      let selected = merge_names ~always_include ~top_names in
      filter_by_names selected tools
    else
      (* Stage 1: BM25 pre-filter *)
      let bm25_top =
        List.filteri (fun i _ -> i < bm25_prefilter_n) retrieved
      in
      (* Build (name, description) candidates for reranker *)
      let candidates =
        List.filter_map (fun (name, _score) ->
          match List.find_opt (fun (t : Tool.t) ->
            t.schema.name = name) tools with
          | Some t -> Some (name, t.schema.description)
          | None -> None
        ) bm25_top
      in
      (* Stage 2: LLM rerank with self-healing fallback *)
      let llm_selected =
        try rerank_fn ~context ~candidates
        with
        | Out_of_memory | Stack_overflow | Sys.Break as exn -> raise exn
        | _exn ->
          (* Self-healing: LLM/network failure -> BM25 top-k *)
          List.filteri (fun i _ -> i < k) bm25_top |> List.map fst
      in
      (* Validate: only keep names that exist in candidates *)
      let candidate_set = Hashtbl.create (List.length candidates) in
      List.iter (fun (n, _) -> Hashtbl.replace candidate_set n true) candidates;
      let valid_names =
        List.filter (fun n -> Hashtbl.mem candidate_set n) llm_selected
      in
      let top_names = List.filteri (fun i _ -> i < k) valid_names in
      let selected = merge_names ~always_include ~top_names in
      filter_by_names selected tools

let select_bm25 ~k ~always_include ~confidence_threshold ~fallback_tools
    ~context ~tools =
  if tools = [] then []
  else
    let index = Tool_index.of_tools tools in
    let retrieved = Tool_index.retrieve index context in
    let top_score = match retrieved with
      | (_, s) :: _ -> s
      | [] -> 0.0
    in
    let use_fallback = match confidence_threshold with
      | Some t -> top_score < t
      | None -> false
    in
    let top_names =
      List.filteri (fun i _ -> i < k) retrieved
      |> List.map fst
    in
    let extra = if use_fallback then fallback_tools else [] in
    let selected_names = merge_names ~always_include ~top_names:(top_names @ extra) in
    filter_by_names selected_names tools

(* ── Public API ──────────────────────────────────────────── *)

let select ~strategy ~context ~tools =
  match strategy with
  | All -> select_all tools
  | TopK_bm25 { k; always_include; confidence_threshold; fallback_tools } ->
    select_bm25 ~k ~always_include ~confidence_threshold ~fallback_tools
      ~context ~tools
  | TopK_llm { k; bm25_prefilter_n; always_include; confidence_threshold;
               rerank_fn } ->
    select_llm ~k ~bm25_prefilter_n ~always_include ~confidence_threshold
      ~rerank_fn ~context ~tools
  | Categorical { classifier = `Llm; _ } ->
    failwith "Tool_selector: Categorical with `Llm classifier not yet implemented (Phase 3)"
  | Categorical { groups; classifier = `Bm25; always_include } ->
    (* BM25 categorical: build index from group names + tool names,
       find matching groups, expose all tools in those groups. *)
    if tools = [] then []
    else
      let group_entries = List.map (fun (group_name, tool_names) ->
        let desc = String.concat " " tool_names in
        { Tool_index.name = group_name; description = desc;
          group = None; aliases = [] }
      ) groups in
      let group_index = Tool_index.build group_entries in
      let matched_groups =
        Tool_index.retrieve_names group_index context
      in
      (* Collect all tool names from matched groups *)
      let group_tool_names = List.concat_map (fun gname ->
        match List.assoc_opt gname groups with
        | Some names -> names
        | None -> []
      ) matched_groups in
      let selected_names = merge_names ~always_include ~top_names:group_tool_names in
      filter_by_names selected_names tools

let select_names ~strategy ~context ~tools =
  List.map (fun (t : Tool.t) -> t.schema.name)
    (select ~strategy ~context ~tools)

let auto ~tools =
  if List.length tools <= 15 then All
  else TopK_bm25 { k = 5; always_include = [];
    confidence_threshold = None; fallback_tools = [] }

(* ── Default rerank function ───────────────────────────── *)

let default_rerank_fn ~sw ~net ?clock ?config_path ~cascade_name ~defaults ~k () =
  fun ~context ~candidates ->
    let tool_list = List.mapi (fun i (name, desc) ->
      Printf.sprintf "%d. %s: %s" (i + 1) name desc
    ) candidates |> String.concat "\n" in
    let prompt = Printf.sprintf
      "Given the user query below, select the %d most relevant tools \
       from the list. Return ONLY tool names, one per line, in order \
       of relevance. No numbering, no explanation.\n\n\
       Query: %s\n\nAvailable tools:\n%s" k context tool_list in
    let messages = [
      { Types.role = Types.User;
        content = [Types.Text prompt];
        name = None; tool_call_id = None }
    ] in
    match Llm_provider.Cascade_config.complete_named ~sw ~net ?clock
        ?config_path ~name:cascade_name ~defaults
        ~messages ~temperature:0.0 ~max_tokens:200 () with
    | Ok response ->
      let text = Types.text_of_response response in
      String.split_on_char '\n' text
      |> List.filter_map (fun line ->
        let trimmed = String.trim line in
        if trimmed = "" then None
        else
          (* Strip leading "1. " or "- " if present *)
          let name =
            if String.length trimmed > 2 then
              match trimmed.[0] with
              | '0'..'9' ->
                (match String.index_opt trimmed '.' with
                 | Some i when i < 4 ->
                   String.trim (String.sub trimmed (i + 1)
                     (String.length trimmed - i - 1))
                 | _ -> trimmed)
              | '-' ->
                String.trim (String.sub trimmed 1
                  (String.length trimmed - 1))
              | _ -> trimmed
            else trimmed
          in
          if List.exists (fun (n, _) -> n = name) candidates
          then Some name
          else None)
    | Error _ ->
      (* Graceful degradation: return candidates in BM25 order *)
      List.filteri (fun i _ -> i < k) (List.map fst candidates)
