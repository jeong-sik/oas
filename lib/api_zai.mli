open Base
(** Z.AI general API helpers for non-chat media endpoints.

    These endpoints are distinct from chat completions and should not be
    modeled as {!Types.api_response}.

    @stability Evolving *)

type async_task_status =
  | Processing
  | Success
  | Fail
  | Unknown of string

type image_result = { url : string }

type image_generation_response =
  { created : int
  ; data : image_result list
  }

type async_submit_response =
  { model : string
  ; id : string
  ; request_id : string option
  ; task_status : async_task_status
  }

type media_result =
  { url : string
  ; cover_image_url : string option
  }

type media_async_result =
  { model : string
  ; task_status : async_task_status
  ; results : media_result list
  ; request_id : string option
  }

type transcription_result =
  { id : string
  ; created : int
  ; request_id : string option
  ; model : string
  ; text : string
  }

type audio_source =
  | File_path of string
  | File_base64 of string

val generate_image
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?base_url:string
  -> ?api_key:string
  -> ?size:string
  -> model:string
  -> prompt:string
  -> unit
  -> (image_generation_response, Llm_provider.Http_client.http_error) result

val generate_image_async
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?base_url:string
  -> ?api_key:string
  -> ?size:string
  -> model:string
  -> prompt:string
  -> unit
  -> (async_submit_response, Llm_provider.Http_client.http_error) result

val generate_video_async
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?base_url:string
  -> ?api_key:string
  -> ?image_url:string
  -> model:string
  -> prompt:string
  -> unit
  -> (async_submit_response, Llm_provider.Http_client.http_error) result

val get_media_async_result
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?base_url:string
  -> ?api_key:string
  -> id:string
  -> unit
  -> (media_async_result, Llm_provider.Http_client.http_error) result

val transcribe_audio
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?base_url:string
  -> ?api_key:string
  -> ?prompt:string
  -> ?language:string
  -> model:string
  -> source:audio_source
  -> unit
  -> (transcription_result, Llm_provider.Http_client.http_error) result
