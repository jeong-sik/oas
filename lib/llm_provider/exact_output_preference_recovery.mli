(** Private evidence validator and constructor for active preference stores. *)

type evidence =
  | Domain_settlement_evidence of Exact_output_domain_settlement.intent
  | Scope_retirement_evidence of Exact_output_scope_retirement.intent

type error =
  | Invalid_concurrent_scope_budget of int
  | Conflicting_domain_settlement_evidence of
      Exact_output_flow_contract.domain_settlement_id
  | Conflicting_scope_retirement_evidence of Exact_output_scope_retirement.id

val recover
  :  concurrent_scope_budget:int
  -> evidence:evidence list
  -> (Exact_output_flow_contract.flow_preference_store, error) result
