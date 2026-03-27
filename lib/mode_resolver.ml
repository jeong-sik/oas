type decision = {
  effective_mode: Execution_mode.t;
  source: string;
}

let min_mode a b =
  if Execution_mode.compare a b <= 0 then a else b

let resolve ~requested ~risk_class ~capabilities:_ =
  match Risk_class.max_mode risk_class with
  | None ->
    Error (Printf.sprintf "risk class %s forbids all execution"
             (Risk_class.to_string risk_class))
  | Some cap ->
    let effective = min_mode requested cap in
    if Execution_mode.equal effective requested then
      Ok { effective_mode = effective; source = "passthrough" }
    else
      Ok { effective_mode = effective; source = "risk_class_downgrade" }
