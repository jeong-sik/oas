type retention = Exact_next_block

type t =
  { retention : retention
  ; payload : Yojson.Safe.t
  }

type malformed_reason =
  | Unsupported_version
  | Unsupported_retention
  | Missing_payload

type decoded =
  | Not_replay
  | Malformed_replay of malformed_reason
  | Replay of t

let schema = "oas.provider_replay"

let encode_exact_next_block ~payload =
  Yojson.Safe.to_string
    (`Assoc
        [ "schema", `String schema
        ; "version", `Int 1
        ; "retention", `String "exact_next_block"
        ; "payload", payload
        ])
;;

let decode data =
  try
    match Yojson.Safe.from_string data with
    | `Assoc fields ->
      (match List.assoc_opt "schema" fields with
       | Some (`String value) when String.equal value schema ->
         (match List.assoc_opt "version" fields with
          | Some (`Int 1) ->
            (match List.assoc_opt "retention" fields with
             | Some (`String "exact_next_block") ->
               (match List.assoc_opt "payload" fields with
                | Some payload -> Replay { retention = Exact_next_block; payload }
                | None -> Malformed_replay Missing_payload)
             | Some _ | None -> Malformed_replay Unsupported_retention)
          | Some _ | None -> Malformed_replay Unsupported_version)
       | Some _ | None -> Not_replay)
    | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> Not_replay
  with
  | Yojson.Json_error _ -> Not_replay
;;
