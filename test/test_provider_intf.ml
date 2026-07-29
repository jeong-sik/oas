(** [Provider.capabilities] / [Llm_provider.Capabilities.capabilities] type
    identity.

    Former Provider_intf module-type-satisfaction and dispatch tests were
    removed (2026-07-21) along with [lib/provider_intf.ml] — that first-class
    -module dispatch island (`of_config`, `PROVIDER`, `STREAMING_PROVIDER`,
    etc.) was fully superseded by [Llm_provider.Complete]. Equivalent HTTP
    dispatch coverage (server-error mapping, malformed-response handling,
    empty-completion-to-ProviderUnavailable, Kimi codec routing) lives in
    [test_complete_http.ml]. *)

open Agent_sdk

(* Compile-time proof that [Provider.capabilities] is the SAME type as
   [Llm_provider.Capabilities.capabilities] — provider.mli exposes the type
   equation. If that equation is dropped, these identity coercions stop
   compiling, which is exactly what forced downstream consumers (catalog
   overlays) to hand-copy every field. *)
let capabilities_as_provider (c : Llm_provider.Capabilities.capabilities)
  : Provider.capabilities
  =
  c
;;

let capabilities_as_source (c : Provider.capabilities)
  : Llm_provider.Capabilities.capabilities
  =
  c
;;

let test_capabilities_type_equality () =
  let c = Provider.default_capabilities in
  (* Round-trips through both directions with no conversion: the values are
     physically identical because the two names denote one type. *)
  let c' = capabilities_as_provider (capabilities_as_source c) in
  Alcotest.(check bool) "capabilities round-trips by identity" true (c == c')
;;

let () =
  Alcotest.run
    "Provider_intf"
    [ ( "capabilities_identity"
      , [ Alcotest.test_case
            "Provider.capabilities = Llm_provider.Capabilities.capabilities"
            `Quick
            test_capabilities_type_equality
        ] )
    ]
;;
