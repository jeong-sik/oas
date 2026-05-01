open Base
(** MCP (Model Context Protocol) client.

    Uses a tolerant NDJSON-over-stdio client for runtime interop while
    keeping the pure SDK bridge helpers for tests. This lets OAS talk to
    MCP servers that paginate [tools/list] and add non-standard fields. *)

open Types
include Mcp_schema
module Sdk_client = Mcp_protocol_eio.Client

(* ── Eio stdio transport client ──────────────────────────────────── *)

type t =
  { client : Sdk_client.t
  ; kill : unit -> unit
  }

(** Extract concatenated text content from a {!Sdk_types.tool_result}. *)
let output_token_budget () = Defaults.int_env_or 25_000 "OAS_MCP_OUTPUT_MAX_TOKENS"

(** Scan backward from [at] to the nearest UTF-8 codepoint boundary so a
    byte-offset truncation never cuts the middle of a multi-byte character.
    Returns the largest index [<= at] whose byte is NOT a UTF-8 continuation
    byte ([0x80-0xBF]). A pure-ASCII cut point is already its own boundary;
    the worst case for well-formed input walks back 1-3 bytes. *)
let utf8_safe_boundary text at =
  let len = String.length text in
  let at = if at > len then len else at in
  let rec scan i =
    if i <= 0
    then 0
    else (
      let b = Char.code (String.unsafe_get text i) in
      if b land 0xC0 <> 0x80 then i else scan (i - 1))
  in
  scan at
;;

(** Truncate [text] when its estimated token count exceeds
    [output_token_budget ()].

    Delegates token estimation to {!Context_reducer.estimate_char_tokens}
    so CJK / emoji content is counted on par with ASCII rather than
    through the previous "1 token ~= 4 bytes" byte-count approximation
    — the old formula over-truncated Korean/Japanese/Chinese tool output
    (3-byte chars × a 4-byte-per-token budget = only half the real
    character budget reachable).

    When truncation is needed we binary-search for the largest prefix
    whose estimated tokens are [<= budget], then snap the cut to a
    UTF-8 codepoint boundary so the output is never a broken
    half-character. For pure-ASCII inputs this reproduces the previous
    [budget * 4] behavior exactly because the estimator rounds 4 ASCII
    chars → 1 token; CJK content now keeps [~budget * 1.5] characters
    instead of [~budget * 1.33 chars but cut mid-codepoint]. *)
let truncate_output text =
  let budget = output_token_budget () in
  if Context_reducer.estimate_char_tokens text <= budget
  then text
  else (
    (* Binary search the largest byte offset whose prefix fits in
       [budget] tokens. The search is in bytes because we do not have
       a cheap way to map byte offsets to codepoint counts without
       scanning, but the per-probe cost is dominated by one call to
       the estimator which is already linear in bytes anyway. *)
    let fits k =
      let safe_k = utf8_safe_boundary text k in
      let prefix = String.sub text 0 safe_k in
      Context_reducer.estimate_char_tokens prefix <= budget
    in
    let rec search lo hi =
      if lo >= hi
      then utf8_safe_boundary text lo
      else (
        let mid = (lo + hi + 1) / 2 in
        if fits mid then search mid hi else search lo (mid - 1))
    in
    let cut = search 0 (String.length text) in
    String.sub text 0 cut ^ "\n...[oas mcp output truncated]")
;;

let text_of_tool_result (r : Sdk_types.tool_result) =
  List.filter_map
    (fun (c : Sdk_types.tool_content) ->
       match c with
       | TextContent { text; _ } -> Some text
       | _ -> None)
    r.content
  |> String.concat "\n"
  |> truncate_output
;;

(** Connect to an MCP server by spawning a subprocess via Eio.
    [sw] controls the process lifetime.  [mgr] spawns the child.
    [command] is the executable path, [args] are command-line arguments.
    [env] optionally overrides the process environment. *)
let connect ~sw ~(mgr : _ Eio.Process.mgr) ~command ~args ?env () =
  try
    let r_child_stdin, w_child_stdin = Eio_unix.pipe sw in
    let r_child_stdout, w_child_stdout = Eio_unix.pipe sw in
    let proc =
      Eio.Process.spawn
        ~sw
        mgr
        ~stdin:(r_child_stdin :> Eio.Flow.source_ty Eio.Resource.t)
        ~stdout:(w_child_stdout :> Eio.Flow.sink_ty Eio.Resource.t)
        ?env
        (command :: args)
    in
    Eio.Flow.close r_child_stdin;
    Eio.Flow.close w_child_stdout;
    let client =
      Sdk_client.create
        ~stdin:(r_child_stdout :> _ Eio.Flow.source)
        ~stdout:(w_child_stdin :> _ Eio.Flow.sink)
        ()
    in
    let kill () =
      try Eio.Process.signal proc Sys.sigterm with
      | Unix.Unix_error _ | Eio.Io _ | Sys_error _ -> ()
    in
    Ok { client; kill }
  with
  | Eio.Io _ as exn ->
    Error (Error.Mcp (ServerStartFailed { command; detail = Printexc.to_string exn }))
  | Unix.Unix_error _ as exn ->
    Error (Error.Mcp (ServerStartFailed { command; detail = Printexc.to_string exn }))
  | Failure msg -> Error (Error.Mcp (ServerStartFailed { command; detail = msg }))
;;

let mcp_tool_of_json = function
  | `Assoc fields ->
    let input_schema =
      match List.assoc_opt "inputSchema" fields with
      | Some schema -> schema
      | None ->
        (match List.assoc_opt "input_schema" fields with
         | Some schema -> schema
         | None -> `Assoc [])
    in
    (match List.assoc_opt "name" fields with
     | Some (`String name) ->
       Some
         { name
         ; description =
             (match List.assoc_opt "description" fields with
              | Some (`String s) -> s
              | _ -> "")
         ; input_schema
         }
     | _ -> None)
    (* skip tools without a valid name *)
  | _ -> None
;;

let initialize t =
  match
    Sdk_client.initialize t.client ~client_name:"oas-mcp-client" ~client_version:"0.10.0"
  with
  | Ok _ -> Ok ()
  | Error detail -> Error (Error.Mcp (InitializeFailed { detail }))
;;

(** Fetch tools from MCP server and return them. *)
let list_tools t =
  match Sdk_client.list_tools_all t.client with
  | Error detail -> Error (Error.Mcp (ToolListFailed { detail }))
  | Ok tools -> Ok (List.map mcp_tool_of_sdk_tool tools)
;;

let decode_items field decode result_json =
  let open Yojson.Safe.Util in
  match result_json |> member field with
  | `List items ->
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | item :: rest ->
        (match decode item with
         | Ok value -> loop (value :: acc) rest
         | Error detail ->
           Error
             (Error.Serialization
                (JsonParseError
                   { detail = Printf.sprintf "MCP %s decode failed: %s" field detail })))
    in
    loop [] items
  | _ -> Ok []
;;

let list_resources t =
  match Sdk_client.list_resources_all t.client with
  | Error detail -> Error (Error.Mcp (ToolListFailed { detail }))
  | Ok resources -> Ok resources
;;

let read_resource t ~uri =
  match Sdk_client.read_resource t.client ~uri with
  | Error detail -> Error (Error.Mcp (ToolCallFailed { tool_name = uri; detail }))
  | Ok contents ->
    Ok
      (List.map
         (fun (content : Sdk_types.resource_contents) ->
            match content.text with
            | Some text -> { content with text = Some (truncate_output text) }
            | None -> content)
         contents)
;;

let list_prompts t =
  match Sdk_client.list_prompts_all t.client with
  | Error detail -> Error (Error.Mcp (ToolListFailed { detail }))
  | Ok prompts -> Ok prompts
;;

let get_prompt t ~name ?(arguments = []) () =
  match Sdk_client.get_prompt t.client ~name ~arguments () with
  | Ok prompt_result -> Ok prompt_result
  | Error detail -> Error (Error.Mcp (ToolCallFailed { tool_name = name; detail }))
;;

(** Invoke a tool on the MCP server.
    Returns the concatenated text content on success. *)
let call_tool t ~name ~arguments : Types.tool_result =
  match Sdk_client.call_tool t.client ~name ~arguments () with
  | Error detail ->
    Error
      { message = Printf.sprintf "MCP tools/call '%s' failed: %s" name detail
      ; recoverable = true
      ; error_class = None
      }
  | Ok result ->
    let text = text_of_tool_result result in
    if Option.value ~default:false result.Sdk_types.is_error
    then Error { message = text; recoverable = true; error_class = None }
    else Ok { content = text }
;;

(** Convert MCP tools to SDK [Tool.t] list.
    Each tool's handler delegates to {!call_tool} on [t]. *)
let to_tools t (tools : mcp_tool list) =
  List.map
    (fun (mt : mcp_tool) ->
       let call_fn input = call_tool t ~name:mt.name ~arguments:input in
       mcp_tool_to_sdk_tool ~call_fn mt)
    tools
;;

(** Check if the MCP server subprocess is still responsive.
    Sends a [ping] request and returns [true] if a response arrives. *)
let is_alive t =
  try
    match Sdk_client.ping t.client with
    | Ok _ -> true
    | Error _ -> false
  with
  | End_of_file | Eio.Io _ | Unix.Unix_error _ | Failure _ -> false
;;

(** Close the MCP client and terminate the subprocess. *)
let close t =
  Sdk_client.close t.client;
  t.kill ()
;;

(* ── Managed lifecycle ─────────────────────────────────────────── *)

(** Server start specification.
    [command] is the executable, [args] its arguments.
    [env] contains extra environment variable overrides (merged with
    the current process environment).  [name] identifies the server. *)
type server_spec =
  { command : string
  ; args : string list
  ; env : (string * string) list
  ; name : string
  }

(** Transport backend for a managed MCP connection. *)
type transport =
  | Stdio of
      { client : t
      ; spec : server_spec
      }
  | Http of
      { close_fn : unit -> unit
      ; base_url : string
      ; headers : (string * string) list
      }

(** A connected MCP server together with its converted SDK tools. *)
type managed =
  { tools : Tool.t list
  ; name : string
  ; transport : transport
  }

(** Merge extra key-value pairs into the current process environment.
    Existing keys listed in [extras] are overridden. *)
let merge_env extras =
  if extras = []
  then Unix.environment ()
  else (
    let extra_keys = List.map fst extras in
    let base_filtered =
      Array.to_list (Unix.environment ())
      |> List.filter (fun entry ->
        let key =
          match String.split_on_char '=' entry with
          | k :: _ -> k
          | [] -> ""
        in
        not (List.mem key extra_keys))
    in
    let extra_entries = List.map (fun (k, v) -> k ^ "=" ^ v) extras in
    Array.of_list (base_filtered @ extra_entries))
;;

(** Close a single managed MCP connection (transport-aware). *)
let close_managed m =
  match m.transport with
  | Stdio { client; _ } ->
    (try close client with
     | Eio.Io _ | Unix.Unix_error _ | Failure _ -> ())
  | Http { close_fn; _ } ->
    (try close_fn () with
     | Eio.Io _ | Unix.Unix_error _ | Failure _ -> ())
;;

(** Close all managed MCP server connections.
    Exceptions from individual servers are swallowed (best-effort). *)
let close_all managed_list = List.iter close_managed managed_list

(** Connect to an MCP server, perform the initialize handshake, fetch
    tools, and convert them to SDK [Tool.t] values.
    On any failure the subprocess is closed before returning [Error]. *)
let connect_and_load ~sw ~mgr spec =
  let env = merge_env spec.env in
  match connect ~sw ~mgr ~command:spec.command ~args:spec.args ~env () with
  | Error e -> Error e
  | Ok client ->
    (try
       match initialize client with
       | Error e ->
         close client;
         Error e
       | Ok () ->
         (match list_tools client with
          | Error e ->
            close client;
            Error e
          | Ok mcp_tools ->
            let tools = to_tools client mcp_tools in
            Ok { tools; name = spec.name; transport = Stdio { client; spec } })
     with
     | Out_of_memory ->
       close client;
       raise Out_of_memory
     | Stack_overflow ->
       close client;
       raise Stack_overflow
     | Sys.Break ->
       close client;
       raise Sys.Break
     | exn ->
       close client;
       Error
         (Error.Mcp
            (InitializeFailed
               { detail =
                   Printf.sprintf "MCP server '%s': %s" spec.name (Printexc.to_string exn)
               })))
;;

(** Connect to multiple MCP servers sequentially.
    If any server fails, all previously-connected servers are closed
    and the first error is returned. *)
let connect_all ~sw ~mgr specs =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | spec :: rest ->
      (match connect_and_load ~sw ~mgr spec with
       | Error e ->
         close_all (List.rev acc);
         Error e
       | Ok m -> loop (m :: acc) rest)
  in
  loop [] specs
;;

(** Reconnect a managed MCP server by closing the old connection
    and starting a fresh one from its spec.
    Returns the new managed value on success.
    Only stdio transports can be reconnected (HTTP has no persistent spec). *)
let reconnect ~sw ~mgr (m : managed) =
  match m.transport with
  | Stdio { client; spec } ->
    (try close client with
     | Eio.Io _ | Unix.Unix_error _ | Failure _ -> ());
    connect_and_load ~sw ~mgr spec
  | Http _ ->
    Error
      (Error.Mcp
         (InitializeFailed
            { detail = Printf.sprintf "Cannot reconnect HTTP MCP server '%s'" m.name }))
;;

(** Connect to multiple MCP servers, returning all that succeed.
    Failed servers are reported in the second element of the pair
    but do not prevent other servers from connecting. *)
let connect_all_best_effort ~sw ~mgr specs =
  let rec loop ok_acc err_acc = function
    | [] -> List.rev ok_acc, List.rev err_acc
    | spec :: rest ->
      (match connect_and_load ~sw ~mgr spec with
       | Error e -> loop ok_acc ((spec.name, e) :: err_acc) rest
       | Ok m -> loop (m :: ok_acc) err_acc rest)
  in
  loop [] [] specs
;;

[@@@coverage off]
(* === Inline tests === *)

let%test "output_token_budget returns default 25000" =
  (* Unset env var to test default *)
  (match Sys.getenv_opt "OAS_MCP_OUTPUT_MAX_TOKENS" with
   | Some _ -> Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" ""
   | None -> ());
  let budget = output_token_budget () in
  budget = 25_000
;;

let%test "truncate_output short string unchanged" =
  (* Ensure default budget *)
  (match Sys.getenv_opt "OAS_MCP_OUTPUT_MAX_TOKENS" with
   | Some _ -> Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" ""
   | None -> ());
  truncate_output "hello" = "hello"
;;

let%test "truncate_output long string gets truncated" =
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "10";
  let s = String.make 200 'x' in
  let result = truncate_output s in
  (* ~10 tokens worth of bytes, snapped to UTF-8 boundary, plus marker *)
  String.length result <= 50 + String.length "\n...[oas mcp output truncated]"
  && String.length result > 0
;;

let%test "truncate_output CJK under budget is unchanged" =
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "1000";
  (* 9 Hangul chars ~= 6 tokens under the CJK-aware estimator,
     well under 1000. *)
  let s =
    "\xEC\x95\x88\xEB\x85\x95\xED\x95\x98\xEC\x84\xB8\xEC\x9A\x94\xEC\x95\x88\xEB\x85\x95\xED\x95\x98\xEC\x84\xB8\xEC\x9A\x94\xEC\x95\x88\xEB\x85\x95\xED\x95\x98"
  in
  truncate_output s = s
;;

let%test "truncate_output CJK over budget snaps to UTF-8 boundary" =
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "2";
  (* Many Hangul chars. 3 bytes each; cut must not land mid-codepoint. *)
  let s = String.concat "" (List.init 40 (fun _ -> "\xEC\x95\x88")) in
  let result = truncate_output s in
  let marker = "\n...[oas mcp output truncated]" in
  let marker_len = String.length marker in
  let body_len = String.length result - marker_len in
  (* (a) marker is present at the end *)
  String.length result >= marker_len
  && String.sub result body_len marker_len = marker
  (* (b) body length is a multiple of 3, i.e. no partial Hangul char *)
  && body_len mod 3 = 0
  (* (c) we actually truncated (result is shorter than input + marker) *)
  && body_len < String.length s
;;

let test_tool_result ?is_error ?structured_content content =
  let fields = [ "content", Sdk_types.tool_content_list_to_yojson content ] in
  let fields =
    match is_error with
    | Some b -> ("isError", `Bool b) :: fields
    | None -> fields
  in
  let fields =
    match structured_content with
    | Some json -> ("structuredContent", json) :: fields
    | None -> fields
  in
  match Sdk_types.tool_result_of_yojson (`Assoc fields) with
  | Ok result -> result
  | Error detail -> failwith ("tool_result_of_yojson failed: " ^ detail)
;;

let%test "text_of_tool_result extracts text content" =
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "10000";
  let r : Sdk_types.tool_result =
    test_tool_result
      [ Sdk_types.TextContent { type_ = "text"; text = "hello"; annotations = None }
      ; Sdk_types.TextContent { type_ = "text"; text = "world"; annotations = None }
      ]
  in
  text_of_tool_result r = "hello\nworld"
;;

let%test "text_of_tool_result empty content" =
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "10000";
  let r : Sdk_types.tool_result = test_tool_result [] in
  text_of_tool_result r = ""
;;

let%test "mcp_tool_of_json valid tool" =
  let json =
    `Assoc
      [ "name", `String "test_tool"
      ; "description", `String "A test tool"
      ; "inputSchema", `Assoc [ "type", `String "object" ]
      ]
  in
  match mcp_tool_of_json json with
  | Some tool -> tool.name = "test_tool" && tool.description = "A test tool"
  | None -> false
;;

let%test "mcp_tool_of_json with input_schema variant" =
  let json =
    `Assoc
      [ "name", `String "tool2"; "input_schema", `Assoc [ "type", `String "object" ] ]
  in
  match mcp_tool_of_json json with
  | Some tool ->
    tool.name = "tool2" && tool.input_schema = `Assoc [ "type", `String "object" ]
  | None -> false
;;

let%test "mcp_tool_of_json missing name returns None" =
  let json = `Assoc [ "description", `String "no name" ] in
  mcp_tool_of_json json = None
;;

let%test "mcp_tool_of_json non-assoc returns None" =
  mcp_tool_of_json (`String "bad") = None
;;

let%test "mcp_tool_of_json name not a string returns None" =
  let json = `Assoc [ "name", `Int 42 ] in
  mcp_tool_of_json json = None
;;

let%test "mcp_tool_of_json missing description defaults to empty" =
  let json = `Assoc [ "name", `String "minimal" ] in
  match mcp_tool_of_json json with
  | Some tool -> tool.description = ""
  | None -> false
;;

let%test "mcp_tool_of_json missing schema defaults to empty assoc" =
  let json = `Assoc [ "name", `String "no_schema" ] in
  match mcp_tool_of_json json with
  | Some tool -> tool.input_schema = `Assoc []
  | None -> false
;;

let%test "merge_env empty extras returns environment unchanged" =
  let env = merge_env [] in
  Array.length env = Array.length (Unix.environment ())
;;

let%test "merge_env adds new entries" =
  let env = merge_env [ "__OAS_TEST_VAR_INLINE__", "test_value" ] in
  Array.exists (fun entry -> entry = "__OAS_TEST_VAR_INLINE__=test_value") env
;;

let%test "merge_env overrides existing keys" =
  let env = merge_env [ "PATH", "/override/path" ] in
  let path_entries =
    Array.to_list env
    |> List.filter (fun e -> String.length e >= 5 && String.sub e 0 5 = "PATH=")
  in
  match path_entries with
  | [ entry ] -> entry = "PATH=/override/path"
  | _ -> false
;;

let%test "decode_items empty list returns Ok []" =
  let json = `Assoc [ "items", `List [] ] in
  decode_items "items" (fun _ -> Ok "x") json = Ok []
;;

let%test "decode_items missing field returns Ok []" =
  let json = `Assoc [] in
  decode_items "items" (fun _ -> Ok "x") json = Ok []
;;

let%test "decode_items decodes successfully" =
  let json = `Assoc [ "items", `List [ `String "a"; `String "b" ] ] in
  match
    decode_items
      "items"
      (fun j ->
         match j with
         | `String s -> Ok s
         | _ -> Error "bad")
      json
  with
  | Ok [ "a"; "b" ] -> true
  | _ -> false
;;

let%test "decode_items propagates decode error" =
  let json = `Assoc [ "items", `List [ `Int 42 ] ] in
  match decode_items "items" (fun _ -> Error "decode failed") json with
  | Error _ -> true
  | Ok _ -> false
;;

(* --- Additional coverage tests --- *)

let%test "output_token_budget respects env var" =
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "100";
  let budget = output_token_budget () in
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "";
  budget = 100
;;

let%test "output_token_budget negative env value falls back to default" =
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "-5";
  let budget = output_token_budget () in
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "";
  budget = 25_000
;;

let%test "output_token_budget non-numeric env value falls back to default" =
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "abc";
  let budget = output_token_budget () in
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "";
  budget = 25_000
;;

let%test "output_token_budget zero falls back to default" =
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "0";
  let budget = output_token_budget () in
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "";
  budget = 25_000
;;

let%test "truncate_output exact boundary not truncated" =
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "5";
  (* 5 tokens * 4 = 20 chars *)
  let s = String.make 20 'a' in
  let result = truncate_output s in
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "";
  result = s
;;

let%test "truncate_output one over boundary is truncated" =
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "5";
  let s = String.make 21 'b' in
  let result = truncate_output s in
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "";
  let suffix = "\n...[oas mcp output truncated]" in
  String.length result < String.length s + String.length suffix
  && String.length result > 0
  && String.sub
       result
       (String.length result - String.length suffix)
       (String.length suffix)
     = suffix
;;

let%test "text_of_tool_result skips non-text content" =
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "10000";
  let r : Sdk_types.tool_result =
    test_tool_result
      [ Sdk_types.ImageContent
          { type_ = "image"; data = "abc"; mime_type = "image/png"; annotations = None }
      ; Sdk_types.TextContent { type_ = "text"; text = "only_text"; annotations = None }
      ]
  in
  let result = text_of_tool_result r in
  Unix.putenv "OAS_MCP_OUTPUT_MAX_TOKENS" "";
  result = "only_text"
;;

let%test "mcp_tool_of_json description not a string defaults to empty" =
  let json = `Assoc [ "name", `String "tool_desc"; "description", `Int 42 ] in
  match mcp_tool_of_json json with
  | Some tool -> tool.description = ""
  | None -> false
;;

let%test "decode_items with single successful item" =
  let json = `Assoc [ "items", `List [ `String "only" ] ] in
  match
    decode_items
      "items"
      (fun j ->
         match j with
         | `String s -> Ok s
         | _ -> Error "bad")
      json
  with
  | Ok [ "only" ] -> true
  | _ -> false
;;

let%test "decode_items field is not a list returns Ok []" =
  let json = `Assoc [ "items", `String "not a list" ] in
  decode_items "items" (fun _ -> Ok "x") json = Ok []
;;

let%test "merge_env multiple overrides" =
  let env = merge_env [ "__OAS_TEST_A__", "val_a"; "__OAS_TEST_B__", "val_b" ] in
  Array.exists (fun e -> e = "__OAS_TEST_A__=val_a") env
  && Array.exists (fun e -> e = "__OAS_TEST_B__=val_b") env
;;
