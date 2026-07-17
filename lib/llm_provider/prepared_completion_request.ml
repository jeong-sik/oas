type t = { request : Llm_transport.completion_request }
type identity = t

type measurement_evidence =
  { request_identity : identity
  ; measurement : Count_tokens_sync.completion_request_measurement
  }

type measured =
  { prepared : t
  ; evidence : measurement_evidence
  }

type 'a prepared_request_use =
  { measurement_evidence : measurement_evidence
  ; value : 'a
  }

let prepare request = { request }
let identity prepared = prepared
let same_identity left right = left == right

let measure ?connection_cache ?clock ?timeout_s ~sw ~net prepared =
  Count_tokens_sync.measure_completion_request
    ?connection_cache
    ?clock
    ?timeout_s
    ~sw
    ~net
    prepared.request
  |> Result.map (fun measurement ->
    { prepared; evidence = { request_identity = prepared; measurement } })
;;

let measurement_evidence measured = measured.evidence

let with_request measured ~f =
  { measurement_evidence = measured.evidence; value = f measured.prepared.request }
;;
