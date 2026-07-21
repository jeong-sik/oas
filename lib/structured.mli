(** Structured output helpers.

    Direct extraction picks its wire from the resolved model's declared
    capabilities: provider-native JSON schema output, the schema carried as a
    single tool's [input_schema], or JSON mode with the schema in the prompt.
    See {!Llm_provider.Structured_output_strategy}. The tool-use helpers below
    remain available to callers driving that wire themselves.

    @stability Evolving
    @since 0.93.1 *)

open Types

(** {1 Schema} *)

type 'a schema =
  { name : string
  ; description : string
  ; params : tool_param list
  ; parse : Yojson.Safe.t -> ('a, string) result
  }

val schema_to_tool_json : _ schema -> Yojson.Safe.t

(** Return the object JSON schema used for provider-native structured output.

    This is the same schema shape embedded under [input_schema] by
    {!schema_to_tool_json}, but without the tool wrapper fields. *)
val schema_to_json_schema : _ schema -> Yojson.Safe.t

val extract_tool_input
  :  schema:'a schema
  -> content_block list
  -> ('a, Error.sdk_error) result

(** {1 Direct extraction} *)

(** Sends one request on the strongest wire the resolved model declares, and
    reads the structured value from the channel that wire uses — response text
    for native schema output and JSON mode, tool arguments for the tool path.
    A model declaring no structured-output wire fails before the request. *)
val extract
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?base_url:string
  -> ?provider:Provider.config
  -> config:agent_config
  -> schema:'a schema
  -> string
  -> ('a, Error.sdk_error) result

(** {1 Extractors} *)

type 'a extractor = api_response -> ('a, string) result

(** Shape requirement for extracting a provider-native structured-output JSON
    payload from an API response. *)
type response_json_shape =
  | Any_json
  | Object_json

(** Extract the response text as JSON using the same provider-native
    structured-output response boundary as {!schema_extractor}. *)
val extract_response_json
  :  ?shape:response_json_shape
  -> api_response
  -> (Yojson.Safe.t, Error.sdk_error) result

(** Agent-level extractor form of {!extract_response_json}. *)
val response_json_extractor
  :  ?shape:response_json_shape
  -> unit
  -> Yojson.Safe.t extractor

(** Build an {!extractor} from a typed schema.

    This is the Agent-level counterpart to {!extract}: it parses the response
    text as JSON, accepts fenced JSON, and delegates shape validation to
    [schema.parse]. *)
val schema_extractor : 'a schema -> 'a extractor

val json_extractor : (Yojson.Safe.t -> 'a) -> 'a extractor
val text_extractor : (string -> 'a option) -> 'a extractor

(** {1 Agent-level structured output} *)

val run_structured
  :  sw:Eio.Switch.t
  -> ?clock:float Eio.Time.clock_ty Eio.Resource.t
  -> Agent.t
  -> string
  -> extract:'a extractor
  -> ('a, Error.sdk_error) result

(** {1 Typed extraction failures} *)

(** Why an extraction produced no value.

    Collapsing these into one string made "this turn called a tool, which is
    the normal shape of a mid-loop turn" indistinguishable from "the provider
    ignored the schema" — the distinction an agent loop has to make, and the
    one a caller needs to tell a contract violation from a refusal or a
    truncation.

    @since 0.220.0 *)
type extraction_failure =
  | No_answer_text_but_tool_calls of { stop_reason : Types.stop_reason }
  | No_answer_text of { stop_reason : Types.stop_reason }
  | Tool_not_called of
      { expected : string
      ; stop_reason : Types.stop_reason
      }
  | Malformed_json of string
  | Schema_mismatch of string

val extraction_failure_to_string : extraction_failure -> string

(** {1 Agent loop} *)

(** Run the agent's tool loop, then take one further turn carrying [schema] on
    whichever wire the resolved model declares.

    Unlike {!run_structured}, the caller neither sets a [response_format] on
    the agent config nor needs the provider to have a native schema field: a
    model without one gets the schema as the terminal turn's single tool, and
    one with neither gets JSON mode plus the schema in the prompt.

    The terminal turn is separate, and carries none of the agent's own tools,
    because the tool strategy cannot be spread across a loop. A schema tool
    exposed on every turn is one the model can call at any point, ending the
    loop early, and it collides with the agent's toolset. Native schema output
    does not have that problem, but the terminal turn is used uniformly so the
    contract does not change with the provider.

    The extra turn costs tokens and is deliberately visible here rather than
    hidden inside the loop.

    @since 0.220.0 *)
val run_structured_schema
  :  sw:Eio.Switch.t
  -> ?clock:float Eio.Time.clock_ty Eio.Resource.t
  -> Agent.t
  -> string
  -> schema:'a schema
  -> ('a, Error.sdk_error) result

(** {1 Streaming extraction} *)

val extract_stream
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?base_url:string
  -> ?provider:Provider.config
  -> ?clock:float Eio.Time.clock_ty Eio.Resource.t
  -> config:agent_config
  -> schema:'a schema
  -> on_event:(sse_event -> unit)
  -> string
  -> ('a * api_response, Error.sdk_error) result
