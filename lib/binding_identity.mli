(** Opaque identity of one resolved OAS provider binding.

    The identity is constructed from the actual resolved provider config and
    OAS-owned catalog facts.  Embedding runtimes may compare/hash it, but cannot
    reconstruct it from display strings or inspect credential material.

    @since 0.211.7 *)

type transport =
  | Http
  | Injected

type t

(** Resolve the transport identity for one concrete call. An explicitly
    injected transport is [Injected]; otherwise it is [Http]. *)
val transport_for_call : injected:bool -> transport

(** Construct an immutable identity from a resolved config. *)
val of_provider_config : transport:transport -> Llm_provider.Provider_config.t -> t

(** Construct from a successfully resolved legacy {!Provider.config} call.
    [base_url], [request_path], and [api_key] are the exact values selected by
    the OAS adapter for that call.  This is the OAS-owned compatibility bridge;
    no embedding runtime reconstructs provider identity from display strings. *)
val of_resolved_provider
  :  transport:transport
  -> provider:Provider.config
  -> base_url:string
  -> request_path:string
  -> api_key:string
  -> t

val equal : t -> t -> bool
val hash : t -> int

(** Whether this binding has a resolved opaque credential identity.  Failure
    attribution uses this fact to fail closed before assigning credential-pool
    ownership. *)
val has_credential_identity : t -> bool

(** Redacted display/telemetry projection.  Raw credentials, full credential
    digests, URI userinfo, and URI query values are never emitted.  This JSON is
    not an equality representation. *)
val to_redacted_yojson : t -> Yojson.Safe.t
