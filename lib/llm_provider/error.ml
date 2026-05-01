open Base
(** Provider-level error types.

    Independent of the OAS sdk_error hierarchy. Can be mapped to
    sdk_error at the boundary or used directly by consumers.

    @since 0.42.0 *)

type provider_error =
  | MissingApiKey of { var_name : string }
  | InvalidConfig of
      { field : string
      ; detail : string
      }
  | ParseError of { detail : string }
  | UnknownVariant of
      { type_name : string
      ; value : string
      }
  | ProviderUnavailable of
      { provider : string
      ; detail : string
      }

let to_string = function
  | MissingApiKey r -> Printf.sprintf "Missing API key env var: %s" r.var_name
  | InvalidConfig r -> Printf.sprintf "Invalid config '%s': %s" r.field r.detail
  | ParseError r -> Printf.sprintf "Parse error: %s" r.detail
  | UnknownVariant r -> Printf.sprintf "Unknown %s variant: %s" r.type_name r.value
  | ProviderUnavailable r ->
    Printf.sprintf "Provider '%s' unavailable: %s" r.provider r.detail
;;
