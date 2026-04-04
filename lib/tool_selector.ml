(** Tool selector: 2-stage tool routing for large tool catalogs.

    Narrows the tool set before the LLM composes arguments, improving
    selection accuracy from ~42% to 83-100% for catalogs with 20+ tools.

    @since 0.100.0 *)

type strategy =
  | All
  | TopK_bm25 of { k: int; always_include: string list }
  | TopK_llm of {
      k: int;
      always_include: string list;
      selector_config: Types.agent_config option;
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

let select_bm25 ~k ~always_include ~context ~tools =
  if tools = [] then []
  else
    let index = Tool_index.of_tools tools in
    let retrieved = Tool_index.retrieve index context in
    let top_names =
      List.filteri (fun i _ -> i < k) retrieved
      |> List.map fst
    in
    let selected_names = merge_names ~always_include ~top_names in
    filter_by_names selected_names tools

(* ── Public API ──────────────────────────────────────────── *)

let select ~strategy ~context ~tools =
  match strategy with
  | All -> select_all tools
  | TopK_bm25 { k; always_include } ->
    select_bm25 ~k ~always_include ~context ~tools
  | TopK_llm _ ->
    failwith "Tool_selector: TopK_llm not yet implemented (Phase 3)"
  | Categorical { classifier = `Llm; _ } ->
    failwith "Tool_selector: Categorical with `Llm classifier not yet implemented (Phase 3)"
  | Categorical { groups; classifier = `Bm25; always_include } ->
    (* BM25 categorical: build index from group names + tool names,
       find matching groups, expose all tools in those groups. *)
    if tools = [] then []
    else
      let group_entries = List.map (fun (group_name, tool_names) ->
        let desc = String.concat " " tool_names in
        { Tool_index.name = group_name; description = desc; group = None }
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
  else TopK_bm25 { k = 5; always_include = [] }
