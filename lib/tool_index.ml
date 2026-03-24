(** Tool index: BM25-based retrieval for dynamic tool exposure.

    Pure OCaml implementation of Okapi BM25, no external dependencies.
    Designed for tool catalogs up to ~500 entries.

    @since 0.89.0 *)

(* ── Tokenization ─────────────────────────────────── *)

(** Simple whitespace + punctuation tokenizer.
    Lowercases and splits on non-alphanumeric boundaries.
    Filters tokens shorter than 2 characters. *)
let tokenize (s : string) : string list =
  let buf = Buffer.create 32 in
  let tokens = ref [] in
  let flush () =
    if Buffer.length buf >= 2 then
      tokens := Buffer.contents buf :: !tokens;
    Buffer.clear buf
  in
  String.iter (fun c ->
    if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then
      Buffer.add_char buf c
    else if c >= 'A' && c <= 'Z' then
      Buffer.add_char buf (Char.lowercase_ascii c)
    else
      flush ()
  ) s;
  flush ();
  List.rev !tokens

(* ── Types ────────────────────────────────────────── *)

type entry = {
  name: string;
  description: string;
  group: string option;
}

type config = {
  k1: float;
  b: float;
  top_k: int;
  min_score: float;
}

let default_config = {
  k1 = 1.5;
  b = 0.75;
  top_k = 10;
  min_score = 0.0;
}

(** Internal document representation *)
type doc = {
  entry: entry;
  tokens: string list;
  token_count: int;
}

type t = {
  config: config;
  docs: doc array;
  avg_dl: float;              (** Average document length *)
  idf: (string, float) Hashtbl.t;  (** Inverse document frequency *)
  total_docs: int;
  vocab_size: int;
}

(* ── IDF computation ──────────────────────────────── *)

(** Compute IDF: log((N - df + 0.5) / (df + 0.5) + 1)
    where N = total docs, df = docs containing term.
    The +1 ensures IDF is always positive. *)
let compute_idf (docs : doc array) : (string, float) Hashtbl.t =
  let n = Array.length docs in
  let df = Hashtbl.create 128 in
  (* Count document frequency for each term *)
  Array.iter (fun doc ->
    let seen = Hashtbl.create 16 in
    List.iter (fun token ->
      if not (Hashtbl.mem seen token) then begin
        Hashtbl.replace seen token true;
        let cur = try Hashtbl.find df token with Not_found -> 0 in
        Hashtbl.replace df token (cur + 1)
      end
    ) doc.tokens
  ) docs;
  (* Convert to IDF *)
  let idf = Hashtbl.create (Hashtbl.length df) in
  Hashtbl.iter (fun term doc_freq ->
    let nf = float_of_int n in
    let dff = float_of_int doc_freq in
    let score = log ((nf -. dff +. 0.5) /. (dff +. 0.5) +. 1.0) in
    Hashtbl.replace idf term score
  ) df;
  idf

(* ── Build ────────────────────────────────────────── *)

let build ?(config = default_config) (entries : entry list) : t =
  let docs = Array.of_list (List.map (fun entry ->
    let text = entry.name ^ " " ^ entry.description in
    let tokens = tokenize text in
    { entry; tokens; token_count = List.length tokens }
  ) entries) in
  let total_docs = Array.length docs in
  let avg_dl =
    if total_docs = 0 then 1.0
    else
      let sum = Array.fold_left (fun acc d -> acc + d.token_count) 0 docs in
      let raw = float_of_int sum /. float_of_int total_docs in
      (* Guard: avg_dl must be > 0 to avoid division by zero in BM25.
         Can be 0.0 if all documents tokenize to empty strings. *)
      if raw < 1.0 then 1.0 else raw
  in
  let idf = compute_idf docs in
  let vocab_size = Hashtbl.length idf in
  { config; docs; avg_dl; idf; total_docs; vocab_size }

let of_tools ?(config = default_config) (tools : Tool.t list) : t =
  let entries = List.map (fun (tool : Tool.t) ->
    { name = tool.schema.name;
      description = tool.schema.description;
      group = None }
  ) tools in
  build ~config entries

(* ── BM25 scoring ─────────────────────────────────── *)

(** Count occurrences of a term in a token list. *)
let term_freq (term : string) (tokens : string list) : int =
  List.fold_left (fun acc t -> if t = term then acc + 1 else acc) 0 tokens

(** Score a single document against query terms. *)
let score_doc (idx : t) (query_tokens : string list) (doc : doc) : float =
  let k1 = idx.config.k1 in
  let b = idx.config.b in
  let dl = float_of_int doc.token_count in
  List.fold_left (fun acc qterm ->
    let idf_val = try Hashtbl.find idx.idf qterm with Not_found -> 0.0 in
    if idf_val <= 0.0 then acc
    else
      let tf = float_of_int (term_freq qterm doc.tokens) in
      let numerator = tf *. (k1 +. 1.0) in
      let denominator = tf +. k1 *. (1.0 -. b +. b *. dl /. idx.avg_dl) in
      acc +. idf_val *. numerator /. denominator
  ) 0.0 query_tokens

(* ── Query ────────────────────────────────────────── *)

let retrieve (idx : t) (query : string) : (string * float) list =
  if idx.total_docs = 0 then []
  else
    let query_tokens = tokenize query in
    if query_tokens = [] then []
    else
      (* Score all documents *)
      let scored = Array.to_list (Array.map (fun doc ->
        (doc.entry, score_doc idx query_tokens doc)
      ) idx.docs) in
      (* Filter by min_score *)
      let filtered = List.filter (fun (_, s) ->
        s > idx.config.min_score
      ) scored in
      (* Sort descending *)
      let sorted = List.sort (fun (_, a) (_, b) ->
        Float.compare b a
      ) filtered in
      (* Take top_k *)
      let top = List.filteri (fun i _ -> i < idx.config.top_k) sorted in
      (* Expand groups: if a matched tool has a group, include all tools
         in that group *)
      let matched_groups = List.filter_map (fun (entry, _) ->
        entry.group
      ) top in
      let group_additions =
        if matched_groups = [] then []
        else
          Array.to_list (Array.to_seq idx.docs
            |> Seq.filter (fun doc ->
              match doc.entry.group with
              | Some g -> List.mem g matched_groups
                          && not (List.exists (fun (e, _) -> e.name = doc.entry.name) top)
              | None -> false)
            |> Array.of_seq)
          |> List.map (fun doc -> (doc.entry, 0.0))
      in
      List.map (fun (entry, score) -> (entry.name, score)) (top @ group_additions)

let retrieve_names (idx : t) (query : string) : string list =
  List.map fst (retrieve idx query)

(* ── Confidence gate ──────────────────────────────── *)

let confident (idx : t) (query : string) ~threshold : bool =
  match retrieve idx query with
  | (_, score) :: _ -> score >= threshold
  | [] -> false

(* ── Stats ────────────────────────────────────────── *)

let size idx = idx.total_docs
let vocabulary_size idx = idx.vocab_size

(* ── Inline tests ─────────────────────────────────── *)

[@@@coverage off]

let%test "tokenize basic" =
  tokenize "Hello World" = ["hello"; "world"]

let%test "tokenize filters short tokens" =
  tokenize "a b cd ef" = ["cd"; "ef"]

let%test "tokenize handles punctuation" =
  tokenize "read_file(path)" = ["read"; "file"; "path"]

let%test "tokenize snake_case" =
  tokenize "search_and_replace" = ["search"; "and"; "replace"]

let%test "empty index" =
  let idx = build [] in
  size idx = 0 && retrieve idx "hello" = []

let%test "single tool retrieval" =
  let idx = build [
    { name = "read_file"; description = "Read contents of a file from disk"; group = None };
  ] in
  match retrieve idx "read file" with
  | [(name, score)] -> name = "read_file" && score > 0.0
  | _ -> false

let%test "ranking order" =
  let idx = build [
    { name = "write_file"; description = "Write content to a file"; group = None };
    { name = "read_file"; description = "Read contents of a file from disk"; group = None };
    { name = "delete_file"; description = "Delete a file from disk"; group = None };
  ] in
  match retrieve idx "read file contents" with
  | (first, _) :: _ -> first = "read_file"
  | _ -> false

let%test "group co-retrieval" =
  let idx = build [
    { name = "git_commit"; description = "Create a git commit"; group = Some "git" };
    { name = "git_push"; description = "Push commits to remote"; group = Some "git" };
    { name = "read_file"; description = "Read a file"; group = None };
  ] in
  let results = retrieve_names idx "commit changes" in
  List.mem "git_commit" results && List.mem "git_push" results

let%test "confident gate" =
  let idx = build [
    { name = "search"; description = "Search for files"; group = None };
  ] in
  confident idx "search files" ~threshold:0.01
  && not (confident idx "completely unrelated quantum physics" ~threshold:10.0)

let%test "min_score filtering" =
  let config = { default_config with min_score = 100.0 } in
  let idx = build ~config [
    { name = "tool"; description = "a tool"; group = None };
  ] in
  retrieve idx "something" = []
