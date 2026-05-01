open Base
(** Multi-stage deterministic correction pipeline.

    @since 0.120.0 *)

(* ── Types ──────────────────────────────────────────────── *)

type correction =
  { stage : string
  ; field : string
  ; from_value : string option
  ; to_value : string
  }

type det_result =
  | Fixed of
      { corrected : Yojson.Safe.t
      ; corrections : correction list
      }
  | Still_invalid of
      { errors : Tool_input_validation.field_error list
      ; attempted : correction list
      }

type stage =
  { name : string
  ; apply : Types.tool_schema -> Yojson.Safe.t -> Yojson.Safe.t
  }

(* ── Stage 1: Type Coercion ─────────────────────────────── *)

(** Delegates to Tool_input_validation.try_coerce for each field.
    Tracks which fields were coerced for correction logging. *)
let coercion_apply (schema : Types.tool_schema) (input : Yojson.Safe.t) : Yojson.Safe.t =
  match input with
  | `Assoc fields ->
    let fields' =
      List.map
        (fun (k, v) ->
           match
             List.find_opt (fun (p : Types.tool_param) -> p.name = k) schema.parameters
           with
           | Some p ->
             (* Always attempt coercion — handles both type mismatches AND
           normalizations like Intlit→Int even when matches_type is true. *)
             (match Tool_input_validation.try_coerce p.param_type v with
              | Some coerced when not (Yojson.Safe.equal coerced v) -> k, coerced
              | _ -> k, v)
           | None -> k, v)
        fields
    in
    `Assoc fields'
  | other -> other
;;

let coercion_stage = { name = "coercion"; apply = coercion_apply }

(* ── Stage 2: Default Injection ─────────────────────────── *)

let zero_default = function
  | Types.String -> `String ""
  | Types.Integer -> `Int 0
  | Types.Number -> `Float 0.0
  | Types.Boolean -> `Bool false
  | Types.Array -> `List []
  | Types.Object -> `Assoc []
;;

let make_default_injection_stage
      ?(default_for = fun (p : Types.tool_param) -> zero_default p.param_type)
      ()
  =
  let apply (schema : Types.tool_schema) (input : Yojson.Safe.t) : Yojson.Safe.t =
    match input with
    | `Assoc fields ->
      let missing_optionals =
        List.filter
          (fun (p : Types.tool_param) ->
             (not p.required) && not (List.mem_assoc p.name fields))
          schema.parameters
      in
      let defaults =
        List.map (fun (p : Types.tool_param) -> p.name, default_for p) missing_optionals
      in
      `Assoc (fields @ defaults)
    | `Null ->
      let defaults =
        List.filter_map
          (fun (p : Types.tool_param) ->
             if p.required then None else Some (p.name, default_for p))
          schema.parameters
      in
      `Assoc defaults
    | other -> other
  in
  { name = "default_injection"; apply }
;;

let default_injection_stage = make_default_injection_stage ()

(* ── Stage 3: Format Normalization ──────────────────────── *)

let make_format_normalization_stage ?(normalize = fun _name s -> String.trim s) () =
  let apply (_schema : Types.tool_schema) (input : Yojson.Safe.t) : Yojson.Safe.t =
    match input with
    | `Assoc fields ->
      let changed = ref false in
      let fields' =
        List.map
          (fun (k, v) ->
             match v with
             | `String s ->
               let result = normalize k s in
               if not (String.equal result s)
               then (
                 changed := true;
                 k, `String result)
               else k, v
             | _ -> k, v)
          fields
      in
      if !changed then `Assoc fields' else input
    | other -> other
  in
  { name = "format_normalization"; apply }
;;

let format_normalization_stage = make_format_normalization_stage ()

(* ── Default pipeline ───────────────────────────────────── *)

let default_stages =
  [ coercion_stage; default_injection_stage; format_normalization_stage ]
;;

(* ── Correction tracking ────────────────────────────────── *)

(** Compare two JSON objects field-by-field and record differences. *)
let diff_corrections ~stage_name (original : Yojson.Safe.t) (corrected : Yojson.Safe.t)
  : correction list
  =
  match original, corrected with
  | `Assoc orig_fields, `Assoc corr_fields ->
    List.filter_map
      (fun (k, new_v) ->
         match List.assoc_opt k orig_fields with
         | Some old_v when not (Yojson.Safe.equal old_v new_v) ->
           Some
             { stage = stage_name
             ; field = k
             ; from_value = Some (Yojson.Safe.to_string old_v)
             ; to_value = Yojson.Safe.to_string new_v
             }
         | None ->
           Some
             { stage = stage_name
             ; field = k
             ; from_value = None
             ; to_value = Yojson.Safe.to_string new_v
             }
         | _ -> None)
      corr_fields
  | _ -> []
;;

(* ── Pipeline execution ─────────────────────────────────── *)

let run ~schema ?(stages = default_stages) input =
  let corrected, rev_corrections =
    List.fold_left
      (fun (current_input, acc_rev) stage ->
         let result = stage.apply schema current_input in
         if result == current_input
         then result, acc_rev
         else (
           let new_corrections =
             diff_corrections ~stage_name:stage.name current_input result
           in
           result, List.rev_append new_corrections acc_rev))
      (input, [])
      stages
  in
  let all_corrections = List.rev rev_corrections in
  match Tool_input_validation.validate schema corrected with
  | Tool_input_validation.Valid final ->
    Fixed { corrected = final; corrections = all_corrections }
  | Tool_input_validation.Invalid errors ->
    Still_invalid { errors; attempted = all_corrections }
;;

(* ── Det/NonDet boundary ────────────────────────────────── *)

let build_nondet_feedback ~tool_name ~args ~still_invalid ~attempted =
  let base_errors =
    Tool_input_validation.format_errors_inline ~tool_name ~args still_invalid
  in
  if attempted = []
  then base_errors
  else (
    let correction_lines =
      List.map
        (fun c ->
           let from =
             match c.from_value with
             | Some v -> v
             | None -> "(added)"
           in
           Printf.sprintf "  [%s] %s: %s -> %s" c.stage c.field from c.to_value)
        attempted
    in
    Printf.sprintf
      "%s\n\nDeterministic corrections attempted (still insufficient):\n%s"
      base_errors
      (String.concat "\n" correction_lines))
;;
