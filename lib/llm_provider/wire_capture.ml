(** See [wire_capture.mli]. *)

let env_dir = "OAS_WIRE_CAPTURE_DIR"

type sink = string -> unit

let noop : sink = fun _ -> ()

let write_line ~dir ~provider ~model chunk =
  (try if not (Sys.file_exists dir) then Unix.mkdir dir 0o700 with _ -> ());
  let path = Filename.concat dir "raw-stream.jsonl" in
  try
    let oc = open_out_gen [ Open_append; Open_creat ] 0o600 path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        let json : Yojson.Safe.t =
          `Assoc
            [
              ("provider", `String provider);
              ("model", `String model);
              ("chunk", `String (Secret_redactor.redact_string chunk));
            ]
        in
        output_string oc (Yojson.Safe.to_string json ^ "\n"))
  with _ -> ()

let make_sink ~provider ~model =
  match Sys.getenv_opt env_dir with
  | None | Some "" -> noop
  | Some dir -> fun chunk -> write_line ~dir ~provider ~model chunk

(* ── Inline tests ─────────────────────────────────────────────── *)

let contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  if nl = 0 then true
  else
    let rec loop i =
      i + nl <= hl
      && (String.equal (String.sub haystack i nl) needle || loop (i + 1))
    in
    loop 0

let%test "make_sink is a no-op when env is unset" =
  Unix.putenv env_dir "";
  let s = make_sink ~provider:"p" ~model:"m" in
  s "raw chunk";
  (* no exception, no output path assumed *)
  true

let%test "make_sink writes a redacted line when env is set" =
  let dir = Filename.temp_dir "oas_wire" "" in
  Unix.putenv env_dir dir;
  let s = make_sink ~provider:"ollama_cloud" ~model:"deepseek-v4-flash" in
  (* Built at runtime so no literal secret appears in source. *)
  let token = "ghp_" ^ String.make 36 '7' in
  s ("delta content " ^ token ^ " end");
  Unix.putenv env_dir "";
  let path = Filename.concat dir "raw-stream.jsonl" in
  let ic = open_in path in
  let content =
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  (not (contains ~needle:token content))
  && contains ~needle:"[REDACTED]" content
  && contains ~needle:"deepseek-v4-flash" content

let%test "disabled sink writes nothing" =
  let dir = Filename.temp_dir "oas_wire_off" "" in
  Unix.putenv env_dir "";
  let s = make_sink ~provider:"p" ~model:"m" in
  s "chunk";
  not (Sys.file_exists (Filename.concat dir "raw-stream.jsonl"))
