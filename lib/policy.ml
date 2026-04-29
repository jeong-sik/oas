(** Policy engine — runtime governance for agent decisions.

    Rules are evaluated in priority order (highest first).
    First matching rule determines the verdict.
    Returns [Allow] if no rule matches.

    @since 0.76.0 *)

type decision_point =
  | BeforeToolCall of
      { tool_name : string
      ; agent_name : string
      }
  | BeforeHandoff of
      { from_agent : string
      ; to_agent : string
      }
  | BeforeResponse of
      { agent_name : string
      ; content_preview : string
      }
  | ResourceRequest of
      { agent_name : string
      ; resource : string
      ; amount : float
      }
  | BeforeMemoryWrite of
      { agent_name : string
      ; tier : string
      ; key : string
      }
  | Custom of
      { name : string
      ; detail : string
      }

type verdict =
  | Allow
  | Deny of string
  | AllowWithCondition of string
  | Escalate of string

type rule =
  { name : string
  ; priority : int
  ; applies_to : decision_point -> bool
  ; evaluate : decision_point -> verdict
  }

type t = { rules : rule list }

type decision =
  { verdict : verdict
  ; matched_rules : rule list
  ; first_match : rule option
  ; policy_source : string
  ; evaluated_at : float
  }

(** Sort rules by priority descending. *)
let sort_rules rules = List.sort (fun a b -> Int.compare b.priority a.priority) rules

let create rules = { rules = sort_rules rules }

let evaluate_with_lineage ?(policy_source = "default") t dp =
  let matched = List.filter (fun r -> r.applies_to dp) t.rules in
  let first =
    match matched with
    | [] -> None
    | r :: _ -> Some r
  in
  let verdict =
    match first with
    | None -> Allow
    | Some r -> r.evaluate dp
  in
  { verdict
  ; matched_rules = matched
  ; first_match = first
  ; policy_source
  ; evaluated_at = Unix.gettimeofday ()
  }
;;

let evaluate t dp =
  let rec find = function
    | [] -> Allow
    | r :: rest -> if r.applies_to dp then r.evaluate dp else find rest
  in
  find t.rules
;;

let add_rule t rule = { rules = sort_rules (rule :: t.rules) }
let remove_rule t name = { rules = List.filter (fun r -> r.name <> name) t.rules }
let rules t = t.rules
let rule_count t = List.length t.rules

let verdict_to_string = function
  | Allow -> "Allow"
  | Deny reason -> Printf.sprintf "Deny(%s)" reason
  | AllowWithCondition cond -> Printf.sprintf "AllowWithCondition(%s)" cond
  | Escalate reason -> Printf.sprintf "Escalate(%s)" reason
;;

let decision_point_to_string = function
  | BeforeToolCall { tool_name; agent_name } ->
    Printf.sprintf "BeforeToolCall(tool=%s, agent=%s)" tool_name agent_name
  | BeforeHandoff { from_agent; to_agent } ->
    Printf.sprintf "BeforeHandoff(from=%s, to=%s)" from_agent to_agent
  | BeforeResponse { agent_name; content_preview } ->
    Printf.sprintf "BeforeResponse(agent=%s, preview=%s)" agent_name content_preview
  | ResourceRequest { agent_name; resource; amount } ->
    Printf.sprintf
      "ResourceRequest(agent=%s, resource=%s, amount=%.2f)"
      agent_name
      resource
      amount
  | BeforeMemoryWrite { agent_name; tier; key } ->
    Printf.sprintf "BeforeMemoryWrite(agent=%s, tier=%s, key=%s)" agent_name tier key
  | Custom { name; detail } -> Printf.sprintf "Custom(%s: %s)" name detail
;;

let decision_to_json d =
  let rule_to_json r = `Assoc [ "name", `String r.name; "priority", `Int r.priority ] in
  let first_match_json =
    match d.first_match with
    | None -> `Null
    | Some r -> rule_to_json r
  in
  `Assoc
    [ "verdict", `String (verdict_to_string d.verdict)
    ; "matched_rules", `List (List.map rule_to_json d.matched_rules)
    ; "first_match", first_match_json
    ; "policy_source", `String d.policy_source
    ; "evaluated_at", `Float d.evaluated_at
    ]
;;
