(** Agent Event Bus -- typed publish/subscribe for agent lifecycle events.

    Each subscriber gets its own bounded {!Eio.Stream.t}; [publish]
    copies each event to every matching subscriber. All state is
    internal to [t] -- no globals.

    @stability Evolving
    @since 0.93.1 *)

(** {2 Event types} *)

type event =
  | AgentStarted of { agent_name: string; task_id: string;
                      session_id: string option; worker_run_id: string option }
  | AgentCompleted of { agent_name: string; task_id: string;
                        result: (Types.api_response, Error.sdk_error) result; elapsed: float;
                        session_id: string option; worker_run_id: string option }
  | ToolCalled of { agent_name: string; tool_name: string; input: Yojson.Safe.t;
                    session_id: string option; worker_run_id: string option }
  | ToolCompleted of { agent_name: string; tool_name: string;
                       output: Types.tool_result;
                       session_id: string option; worker_run_id: string option }
  | TurnStarted of { agent_name: string; turn: int;
                     session_id: string option; worker_run_id: string option }
  | TurnCompleted of { agent_name: string; turn: int;
                       session_id: string option; worker_run_id: string option }
  | ElicitationCompleted of { agent_name: string; question: string;
                              response: Hooks.elicitation_response }
  | TaskStateChanged of { task_id: string; from_state: string; to_state: string }
  | ContextCompacted of { agent_name: string; before_tokens: int;
                          after_tokens: int; phase: string;
                          session_id: string option; worker_run_id: string option }
  | Custom of string * Yojson.Safe.t

(** {2 Bus} *)

type t

val create : ?buffer_size:int -> unit -> t

(** {2 Filters} *)

type filter = event -> bool

val accept_all : filter
val filter_agent : string -> filter
val filter_tools_only : filter
val filter_topic : string -> filter
val filter_any : filter list -> filter
val filter_all : filter list -> filter

(** {2 Subscription} *)

type subscription

val subscribe : ?filter:filter -> t -> subscription
val unsubscribe : t -> subscription -> unit

(** {2 Publish and drain} *)

val publish : t -> event -> unit
val drain : subscription -> event list

(** {2 Queries} *)

val subscriber_count : t -> int
