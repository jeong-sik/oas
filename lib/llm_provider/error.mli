(** Provider-level error types.

    Independent of the OAS [Error.sdk_error] hierarchy. *)

type provider_error =
  | MissingApiKey of { var_name: string }
  | InvalidConfig of { field: string; detail: string }
  | ParseError of { detail: string }
  | UnknownVariant of { type_name: string; value: string }
  | ProviderUnavailable of { provider: string; detail: string }

val to_string : provider_error -> string
