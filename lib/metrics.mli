(** SDK metrics collection — counters and histograms.

    Instance-based: each agent can maintain independent metrics.
    Follows OpenTelemetry semantic conventions for gen_ai metrics.

    Usage:
    {[
      let m = Metrics.create ()
      let tok = Metrics.counter m ~name:"gen_ai.client.token.usage" ~unit_:"token"
      Metrics.incr tok ~labels:[("gen_ai.token.type", "input")] 150
      Metrics.to_otlp_json m  (* export all collected metrics *)
    ]} *)

(** {1 Types} *)

type t
(** Metrics instance. *)

type counter
(** Monotonically increasing integer counter. *)

type histogram
(** Distribution of float observations. *)

(** {1 Construction} *)

val create : unit -> t

val counter : t -> name:string -> unit_:string -> counter
(** Register or retrieve a counter by name. *)

val histogram : t -> name:string -> buckets:float list -> histogram
(** Register or retrieve a histogram by name. *)

(** {1 Recording} *)

val incr : counter -> ?labels:(string * string) list -> int -> unit
(** Increment a counter. *)

val observe : histogram -> float -> unit
(** Record an observation in a histogram. *)

(** {1 Export} *)

val to_otlp_json : t -> Yojson.Safe.t
(** Export all metrics in OTLP JSON format. *)

(** {1 Inspection} *)

val counter_value : counter -> ?labels:(string * string) list -> unit -> int
(** Read current counter value for given labels. *)

val histogram_count : histogram -> int
(** Number of observations recorded. *)

val reset : t -> unit
(** Clear all recorded data. *)
