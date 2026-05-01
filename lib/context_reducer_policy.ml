open Base
let clamp_score score = Float.min 1.0 (Float.max 0.0 score)

let filter_importance ?(threshold = 0.3) ?boost ~scorer messages =
  let threshold = clamp_score threshold in
  let total = List.length messages in
  List.filteri
    (fun index message ->
       let base_score = clamp_score (scorer ~index ~total message) in
       let final_score =
         match boost with
         | None -> base_score
         | Some boost_fn ->
           (match boost_fn message with
            | None -> base_score
            | Some boosted -> Float.max base_score (clamp_score boosted))
       in
       final_score >= threshold)
    messages
;;
