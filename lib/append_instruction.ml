(** Append Instruction — hook-based dynamic instruction injection.

    @since 0.55.0 *)

(* ── Types ───────────────────────────────────────────────────────── *)

type instruction_source =
  | Static of string
  | FromContext of string
  | FromFile of string
  | Dynamic of (int -> string option)

type config = {
  sources: instruction_source list;
}

(* ── Source rendering ────────────────────────────────────────────── *)

let render_source ?context ~turn = function
  | Static s -> Some s
  | FromContext key ->
    (match context with
     | Some ctx ->
       (match Context.get ctx key with
        | Some json ->
          (match json with
           | `String s -> Some s
           | other -> Some (Yojson.Safe.to_string other))
        | None -> None)
     | None -> None)
  | FromFile path ->
    (try
       let ic = open_in_bin path in
       Fun.protect
         ~finally:(fun () -> close_in_noerr ic)
         (fun () -> Some (really_input_string ic (in_channel_length ic)))
     with exn ->
       let _ = Printf.eprintf "[append_instruction] FromFile %s failed: %s\n%!"
         path (Printexc.to_string exn) in
       None)
  | Dynamic f -> f turn

(* ── Public API ──────────────────────────────────────────────────── *)

let render ?context ~turn config =
  let parts =
    List.filter_map (render_source ?context ~turn) config.sources
  in
  match parts with
  | [] -> None
  | _ -> Some (String.concat "\n\n" parts)

let as_hook ?context config : Hooks.hook =
  fun event ->
    match event with
    | Hooks.BeforeTurnParams { turn; current_params; _ } ->
      (match render ?context ~turn config with
       | None -> Continue
       | Some text ->
         let existing =
           match current_params.extra_system_context with
           | Some prev -> prev ^ "\n\n" ^ text
           | None -> text
         in
         AdjustParams { current_params with
                        extra_system_context = Some existing })
    | _ -> Continue
