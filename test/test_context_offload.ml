(** Tests for Context_offload — offloading large tool results to files. *)

open Agent_sdk

let check_string = Alcotest.(check string)
let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)

(* ── default_config ───────────────────────────────────── *)

let test_default_config () =
  let c = Context_offload.default_config in
  check_int "threshold" 4096 c.threshold_bytes;
  check_int "preview_len" 200 c.preview_len;
  check_bool "output_dir non-empty" true (String.length c.output_dir > 0)
;;

(* ── maybe_offload: below threshold ───────────────────── *)

let test_kept_below_threshold () =
  let config : Context_offload.config =
    { threshold_bytes = 100
    ; output_dir = Filename.get_temp_dir_name ()
    ; preview_len = 20
    }
  in
  let content = "short content" in
  match Context_offload.maybe_offload ~config ~tool_name:"test" content with
  | Context_offload.Kept c -> check_string "unchanged" content c
  | Context_offload.Offloaded _ -> Alcotest.fail "should not offload"
;;

let test_kept_at_exact_threshold () =
  let content = String.make 50 'x' in
  let config : Context_offload.config =
    { threshold_bytes = 50; output_dir = Filename.get_temp_dir_name (); preview_len = 10 }
  in
  match Context_offload.maybe_offload ~config ~tool_name:"test" content with
  | Context_offload.Kept _ -> ()
  | Context_offload.Offloaded _ -> Alcotest.fail "at threshold should keep"
;;

(* ── maybe_offload: above threshold ───────────────────── *)

let test_offloaded_above_threshold () =
  let content = String.make 200 'z' in
  let config : Context_offload.config =
    { threshold_bytes = 50; output_dir = Filename.get_temp_dir_name (); preview_len = 20 }
  in
  match Context_offload.maybe_offload ~config ~tool_name:"my_tool" content with
  | Context_offload.Offloaded { path; preview; original_bytes } ->
    check_int "original_bytes" 200 original_bytes;
    check_int "preview length" 20 (String.length preview);
    check_bool
      "path contains tool name"
      true
      (Util.string_contains ~needle:"my_tool" path);
    (* Clean up *)
    (try Sys.remove path with
     | _ -> ())
  | Context_offload.Kept _ -> Alcotest.fail "should offload"
;;

(* ── maybe_offload: fail-open on bad dir ──────────────── *)

let test_failopen_bad_dir () =
  let content = String.make 200 'a' in
  let output_dir = Filename.temp_file "oas-context-offload-not-dir" ".txt" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove output_dir with
      | Sys_error _ -> ())
    (fun () ->
       let config : Context_offload.config =
         { threshold_bytes = 50; output_dir; preview_len = 10 }
       in
       let warnings = ref [] in
       let result =
         Llm_provider.Diag.with_sink
           (fun level ~ctx msg -> warnings := (level, ctx, msg) :: !warnings)
           (fun () -> Context_offload.maybe_offload ~config ~tool_name:"test" content)
       in
       (match result with
        | Context_offload.Kept c -> check_int "original content" 200 (String.length c)
        | Context_offload.Offloaded _ -> Alcotest.fail "should fail open");
       check_bool
         "warns"
         true
         (List.exists
            (fun (level, ctx, _msg) ->
               level = Llm_provider.Diag.Warn && String.equal ctx Context_offload.diag_ctx)
            !warnings))
;;

(* ── to_context_string ────────────────────────────────── *)

let test_to_context_string_kept () =
  let result = Context_offload.to_context_string (Kept "hello") in
  check_string "kept unchanged" "hello" result
;;

let test_to_context_string_offloaded () =
  let result =
    Context_offload.to_context_string
      (Offloaded { path = "/tmp/f.txt"; preview = "prev"; original_bytes = 5000 })
  in
  check_bool "contains path" true (Util.string_contains ~needle:"/tmp/f.txt" result);
  check_bool "contains bytes" true (Util.string_contains ~needle:"5000" result);
  check_bool "contains preview" true (Util.string_contains ~needle:"prev" result)
;;

(* ── offload_tool_result (convenience) ────────────────── *)

let test_offload_tool_result_small () =
  let config : Context_offload.config =
    { threshold_bytes = 1000
    ; output_dir = Filename.get_temp_dir_name ()
    ; preview_len = 50
    }
  in
  let result = Context_offload.offload_tool_result ~config ~tool_name:"t" "small" in
  check_string "small unchanged" "small" result
;;

(* ── tool_name sanitization / uniqueness ──────────────── *)

let test_tool_name_uses_store_sanitizer () =
  let content = String.make 200 'b' in
  let config : Context_offload.config =
    { threshold_bytes = 50; output_dir = Filename.get_temp_dir_name (); preview_len = 10 }
  in
  match
    Context_offload.maybe_offload ~config ~tool_name:"../ns/tool:bad\\..\000 name" content
  with
  | Context_offload.Offloaded { path; _ } ->
    let basename = Filename.basename path in
    check_bool
      "contains sanitized tool name"
      true
      (Util.string_contains ~needle:"nstoolbadname" basename);
    check_bool "no slash in filename" false (Util.string_contains ~needle:"/" basename);
    check_bool "no colon in filename" false (Util.string_contains ~needle:":" basename);
    check_bool
      "no backslash in filename"
      false
      (Util.string_contains ~needle:"\\" basename);
    check_bool "not hidden" true (String.length basename > 0 && basename.[0] <> '.');
    (try Sys.remove path with
     | _ -> ())
  | Context_offload.Kept _ -> Alcotest.fail "should offload"
;;

let test_offload_paths_unique_for_same_tool () =
  let content = String.make 200 'c' in
  let config : Context_offload.config =
    { threshold_bytes = 50; output_dir = Filename.get_temp_dir_name (); preview_len = 10 }
  in
  match
    ( Context_offload.maybe_offload ~config ~tool_name:"same_tool" content
    , Context_offload.maybe_offload ~config ~tool_name:"same_tool" content )
  with
  | ( Context_offload.Offloaded { path = path1; _ }
    , Context_offload.Offloaded { path = path2; _ } ) ->
    check_bool "unique paths" true (not (String.equal path1 path2));
    (try Sys.remove path1 with
     | _ -> ());
    (try Sys.remove path2 with
     | _ -> ())
  | Context_offload.Kept _, _ | _, Context_offload.Kept _ ->
    Alcotest.fail "should offload"
;;

let test_tool_name_without_safe_chars_fails_open () =
  let content = String.make 200 'd' in
  let config : Context_offload.config =
    { threshold_bytes = 50; output_dir = Filename.get_temp_dir_name (); preview_len = 10 }
  in
  let warnings = ref [] in
  let result =
    Llm_provider.Diag.with_sink
      (fun level ~ctx msg -> warnings := (level, ctx, msg) :: !warnings)
      (fun () -> Context_offload.maybe_offload ~config ~tool_name:"../:\\ \000" content)
  in
  (match result with
   | Context_offload.Kept c -> check_int "original content" 200 (String.length c)
   | Context_offload.Offloaded _ -> Alcotest.fail "should fail open");
  check_bool
    "warns"
    true
    (List.exists
       (fun (level, ctx, _msg) ->
          level = Llm_provider.Diag.Warn && String.equal ctx Context_offload.diag_ctx)
       !warnings)
;;

(* ── Suite ────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "context_offload"
    [ "default_config", [ Alcotest.test_case "default values" `Quick test_default_config ]
    ; ( "maybe_offload"
      , [ Alcotest.test_case "below threshold" `Quick test_kept_below_threshold
        ; Alcotest.test_case "at threshold" `Quick test_kept_at_exact_threshold
        ; Alcotest.test_case "above threshold" `Quick test_offloaded_above_threshold
        ; Alcotest.test_case "fail-open bad dir" `Quick test_failopen_bad_dir
        ; Alcotest.test_case
            "tool name uses store sanitizer"
            `Quick
            test_tool_name_uses_store_sanitizer
        ; Alcotest.test_case
            "same tool gets unique paths"
            `Quick
            test_offload_paths_unique_for_same_tool
        ; Alcotest.test_case
            "unsafe-only tool name fails open"
            `Quick
            test_tool_name_without_safe_chars_fails_open
        ] )
    ; ( "to_context_string"
      , [ Alcotest.test_case "kept" `Quick test_to_context_string_kept
        ; Alcotest.test_case "offloaded" `Quick test_to_context_string_offloaded
        ] )
    ; ( "convenience"
      , [ Alcotest.test_case
            "offload_tool_result small"
            `Quick
            test_offload_tool_result_small
        ] )
    ]
;;
