(** How a structured-output request is expressed on the wire for a given
    provider/model.

    OAS previously treated structured output as a boolean: either the provider
    has a native JSON-schema request field or the request is rejected. A
    2026-07-22 sweep of every provider's official documentation shows that
    dichotomy loses real capability. Providers with no native schema field —
    Z.AI GLM, DeepSeek, Cohere, MiniMax, Ollama Cloud — all support tools, and
    a tool's [input_schema] is a JSON Schema the provider constrains the model
    against. DeepSeek even validates it server-side under
    [tools[].function.strict]. The path exists; the boolean hid it.

    Selection is a pure function of declared capabilities. It never inspects a
    host, a URL, or the syntax of a model id (RFC-OAS-034 §2), and it is a
    *selection*, not a runtime fallback cascade: OAS does not send a native
    request, observe a rejection, and retry with a different wire shape. Every
    multi-provider SDK surveyed (Vercel AI SDK, LangChain, Instructor,
    openclaw) selects up front for the same reason — a cascade doubles cost
    and hides which wire actually failed.

    @stability Internal
    @since 0.220.0 *)

(** How the schema-carrying tool is selected on a {!Tool_call} request.

    Derived from capabilities rather than assumed, because the providers that
    most need this strategy are also the ones with the weakest [tool_choice]
    support: Z.AI GLM accepts only [auto], and Cohere has no [tool_choice]
    parameter at all. *)
type tool_selection =
  | Forced_named (** [tool_choice] names the schema tool. *)
  | Forced_any
  (** [tool_choice] requires some tool. Sound because the request carries
      exactly one. *)
  | Model_choice
  (** No usable [tool_choice]. The request carries exactly one tool and the
      prompt asks for it. This is the only path openclaw uses, and it is what
      covers GLM and Cohere. *)

type t =
  | Native_json_schema
  (** The provider constrains decoding to the schema through a request field.
      The field shape is a provider dialect concern
      ([response_format.json_schema], [text.format], [output_config.format],
      [generationConfig.responseFormat], Ollama's [format]) and is decided by
      the backend, not here. *)
  | Tool_call of tool_selection
  (** The schema rides a single tool's [input_schema]. The model calls the
      tool and its arguments are the structured output. *)
  | Json_mode_with_prompt_schema
  (** [response_format = json_object] plus the schema in the prompt. The
      provider guarantees JSON syntax but not the shape, so the caller's
      parser is the only conformance check. Last resort. *)

type unsupported =
  | No_structured_output_path
  (** The model declares neither schema-constrained output, nor tools, nor a
      JSON output mode. There is nothing to degrade to, so the request fails
      rather than pretending. *)

val unsupported_to_string : unsupported -> string
val to_string : t -> string

(** Pick the strongest available strategy.

    Order is by guarantee strength, not convenience:
    [supports_structured_output] → {!Native_json_schema};
    else [supports_tools] → {!Tool_call};
    else [supports_response_format_json] → {!Json_mode_with_prompt_schema};
    else [Error]. The match is exhaustive with no catch-all, so a new
    capability that widens this space forces a decision here. *)
val select : capabilities:Capabilities.capabilities -> (t, unsupported) result

(** The [tool_choice] a {!Tool_call} request should carry, or [None] when the
    model has no usable forced-choice mode and the single-tool request must
    rely on the prompt. *)
val tool_choice_of_selection : tool_selection -> Types.tool_choice option
