(** Succession — cross-agent context relay for multi-generation execution.

    Compresses an agent's working context into a structured DNA payload
    that a successor agent can hydrate, even on a different model.

    Supports the perpetual agent pattern: when context fills up, extract
    DNA (goal, progress, decisions, pending actions), hand off to a new
    agent instance, and continue with compressed history.

    @since 0.79.0 *)

(** {1 Metrics} *)

type metrics = {
  total_turns : int;
  total_tokens_used : int;
  total_cost_usd : float;
  tasks_completed : int;
  errors_encountered : int;
  elapsed_seconds : float;
}

val empty_metrics : metrics
val merge_metrics : metrics -> metrics -> metrics

(** {1 DNA — compressed handoff payload} *)

type dna = {
  generation : int;
  trace_id : string;
  goal : string;
  progress_summary : string;
  compressed_context : string;
  pending_actions : string list;
  key_decisions : string list;
  memory_refs : string list;
  warnings : string list;
  metrics : metrics;
}

(** {1 DNA Extraction} *)

(** Extract DNA from conversation messages and goal.

    Analyzes messages to produce:
    - progress_summary: what has been accomplished
    - pending_actions: incomplete tool calls, unanswered questions
    - key_decisions: significant choices made during execution
    - compressed_context: truncated message history *)
val extract_dna :
  messages:Types.message list ->
  goal:string ->
  generation:int ->
  trace_id:string ->
  ?metrics:metrics ->
  ?warnings:string list ->
  unit -> dna

(** {1 Hydration — restore context from DNA} *)

(** Build a system prompt incorporating DNA context for the successor. *)
val build_successor_system_prompt : dna -> string

(** Hydrate DNA into a message list suitable for a new agent.
    Includes a summary of previous progress as the initial context. *)
val hydrate_messages : dna -> Types.message list

(** {1 Cross-model normalization} *)

(** Normalize messages for a target model.
    Repairs tool call pairing, strips thinking blocks,
    and ensures role alternation. *)
val normalize_for_model :
  Types.message list ->
  target_model:string ->
  Types.message list

(** {1 Serialization} *)

val dna_to_json : dna -> Yojson.Safe.t
val dna_of_json : Yojson.Safe.t -> (dna, string) result
val metrics_to_json : metrics -> Yojson.Safe.t
val metrics_of_json : Yojson.Safe.t -> metrics
