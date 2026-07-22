(** Projection of a caller JSON schema into the OpenAI Structured Outputs
    strict subset.

    OpenAI's strict mode is not "the schema plus a flag": sending
    [strict: true] alongside a schema that misses any of the subset's
    structural requirements is rejected with HTTP 400 before the model runs.
    Measured against [api.openai.com/v1/chat/completions] with [gpt-5.5] on
    2026-07-22:

    {v
    schema {type:object, properties:{city:{type:string}}, required:["city"]}
      + strict:true
      -> 400 "Invalid schema for response_format 'structured_output':
              In context=(), 'additionalProperties' is required to be
              supplied and to be false."

    same schema + additionalProperties:false + every property in required
      -> 200

    {type:array} property with no [items]
      -> 400 "In context=('properties','tags'), array schema missing items."
    v}

    OAS builds structured-output schemas from {!Types.params_to_input_schema},
    which emits neither [additionalProperties] nor array [items]. Every
    {!Structured.extract} call against an OpenAI-family endpoint therefore
    failed at the wire boundary. This module is the single place that decides,
    per schema, whether OAS may honestly request the strict guarantee.

    Two documented subset rules are applied as rewrites rather than
    rejections, because both are lossless for consumers:

    - [additionalProperties: false] is injected on an object only when the
      caller left the keyword unset — the caller declared no extra keys, so
      forbidding them changes nothing. An explicit [false] is kept; an explicit
      [true] or schema value is an open-object declaration the subset cannot
      express and is reported as {!Object_open_additional_properties} rather
      than overwritten.
    - Properties absent from [required] are promoted into [required] and made
      nullable, the optional-field emulation OpenAI documents ("it is possible
      to emulate an optional parameter by using a union type with null"). A
      property with a top-level [type] gets [null] added to it; one without a
      usable [type] ([enum]/[const]/[$ref]/[anyOf]) is wrapped so a [null]
      branch is reachable. Consumers reading the payload with
      [Yojson.Safe.Util.member] observe [`Null] for both an absent key and an
      explicit null, so the promotion does not change what a parser sees.

    Requirements that cannot be met by rewriting are reported as
    {!violation}s. OAS never invents the missing information — an array whose
    element type the caller never declared has no correct [items], and
    guessing one would silently constrain the model to a shape the caller did
    not ask for. Callers of {!project} degrade such a request to non-strict
    [json_schema], which the same endpoint accepts and serves best-effort
    (measured 200 on 2026-07-22), and surface the violation so the caller can
    enrich the schema and regain the guarantee.

    Ref: OpenAI Structured Outputs guide, "Supported schemas" —
    https://developers.openai.com/api/docs/guides/structured-outputs —
    checked 2026-07-22.

    @stability Internal
    @since 0.220.0 *)

(** Location of a violation inside the caller schema, as a dotted path from
    the schema root. The root itself is ["<root>"]. *)
type path = string

type violation =
  | Array_without_items of path
  (** An [{"type":"array"}] schema with no [items] keyword. The strict subset
      requires the element schema; OAS cannot supply one. *)
  | Object_without_properties of path
  (** An [{"type":"object"}] schema with no [properties] keyword. The strict
      subset requires [additionalProperties:false], which on a
      property-less object would constrain the model to the empty object —
      almost never what the caller meant, so this is reported instead of
      rewritten. *)
  | Root_not_object of path
  (** The schema root is not [{"type":"object"}]. Verbatim from the guide:
      "the root level object of a schema must be an object, and not use
      anyOf". *)
  | Object_open_additional_properties of path
  (** An object declares [additionalProperties] that permits extra keys —
      [true], or a schema value. The strict subset requires
      [additionalProperties:false], so this cannot be honored strictly; rather
      than silently overwriting the caller's open-object intent, the request
      degrades to non-strict with the schema left as the caller wrote it. *)

val violation_to_string : violation -> string

(** [", "]-joined {!violation_to_string}, stable order (input order). *)
val violations_to_string : violation list -> string

(** [project schema] returns the strict-subset form of [schema], or every
    structural requirement that could not be satisfied by rewriting.

    Recurses through [properties], [items], [anyOf], [allOf], [oneOf] and
    [$defs]. Keywords the projection does not recognize are preserved
    verbatim: a caller-supplied schema may legitimately use subset features
    (pattern, enum, const, format, numeric bounds) that need no rewrite, and
    silently dropping them would change the request. This means {!project}
    returning [Ok] is a statement about the requirements it checks, not a
    proof that OpenAI will accept the schema; an unrecognized-but-unsupported
    keyword still surfaces as the provider's own 400, which names the exact
    context. *)
val project : Yojson.Safe.t -> (Yojson.Safe.t, violation list) result
