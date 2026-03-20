(** Statistical utilities for eval regression detection.

    @since 0.79.0 *)

type stats = {
  n: int;
  mean: float;
  std_dev: float;
  min_val: float;
  max_val: float;
}

let sum xs = List.fold_left ( +. ) 0.0 xs

let mean xs =
  let n = List.length xs in
  if n = 0 then 0.0
  else sum xs /. float_of_int n

let variance xs =
  let n = List.length xs in
  if n < 2 then 0.0
  else
    let m = mean xs in
    let ss = List.fold_left (fun acc x -> acc +. (x -. m) *. (x -. m)) 0.0 xs in
    ss /. float_of_int (n - 1)

let std_dev xs = sqrt (variance xs)

let summary_stats xs =
  match xs with
  | [] -> None
  | _ ->
    let n = List.length xs in
    let m = mean xs in
    let sd = std_dev xs in
    let min_val = List.fold_left Float.min Float.max_float xs in
    let max_val = List.fold_left Float.max Float.min_float xs in
    Some { n; mean = m; std_dev = sd; min_val; max_val }

(** Z-score for common confidence levels (normal approximation). *)
let z_score confidence =
  if confidence >= 0.99 then 2.576
  else if confidence >= 0.95 then 1.960
  else if confidence >= 0.90 then 1.645
  else 1.0

let confidence_interval xs ~confidence =
  let n = List.length xs in
  if n < 2 then None
  else
    let m = mean xs in
    let se = std_dev xs /. sqrt (float_of_int n) in
    let z = z_score confidence in
    Some (m -. z *. se, m +. z *. se)

(** Welch's t-test: tests whether current is significantly higher than baseline.
    One-sided test at alpha=0.05.
    Uses approximate critical value t=1.645 (conservative for df > 30). *)
let is_regression ~baseline ~current =
  let n1 = List.length baseline in
  let n2 = List.length current in
  if n1 < 2 || n2 < 2 then false
  else
    let m1 = mean baseline in
    let m2 = mean current in
    let v1 = variance baseline in
    let v2 = variance current in
    let se = sqrt (v1 /. float_of_int n1 +. v2 /. float_of_int n2) in
    if se < 1e-10 then
      m2 > m1  (* Zero variance: any increase is a regression *)
    else
      let t = (m2 -. m1) /. se in
      t > 1.645  (* One-sided alpha=0.05 *)

let effect_size xs ys =
  let n1 = List.length xs in
  let n2 = List.length ys in
  if n1 < 2 || n2 < 2 then None
  else
    let m1 = mean xs in
    let m2 = mean ys in
    let v1 = variance xs in
    let v2 = variance ys in
    let pooled_var =
      (float_of_int (n1 - 1) *. v1 +. float_of_int (n2 - 1) *. v2)
      /. float_of_int (n1 + n2 - 2)
    in
    let pooled_sd = sqrt pooled_var in
    if pooled_sd < 1e-10 then None
    else Some ((m2 -. m1) /. pooled_sd)

[@@@coverage off]
(* === Inline tests === *)

let%test "summary_stats empty" =
  summary_stats [] = None

let%test "summary_stats single element" =
  match summary_stats [5.0] with
  | Some s -> s.n = 1 && s.mean = 5.0 && s.min_val = 5.0 && s.max_val = 5.0
  | None -> false

let%test "summary_stats multiple" =
  match summary_stats [1.0; 2.0; 3.0; 4.0; 5.0] with
  | Some s -> s.n = 5 && s.mean = 3.0 && s.min_val = 1.0 && s.max_val = 5.0
  | None -> false

let%test "summary_stats std_dev" =
  match summary_stats [2.0; 4.0; 4.0; 4.0; 5.0; 5.0; 7.0; 9.0] with
  | Some s -> s.std_dev > 2.0 && s.std_dev < 2.2  (* ~2.138 *)
  | None -> false

let%test "confidence_interval too small" =
  confidence_interval [1.0] ~confidence:0.95 = None

let%test "confidence_interval 95%" =
  match confidence_interval [1.0; 2.0; 3.0; 4.0; 5.0] ~confidence:0.95 with
  | Some (lo, hi) -> lo < 3.0 && hi > 3.0 && lo > 0.0 && hi < 6.0
  | None -> false

let%test "is_regression no data" =
  is_regression ~baseline:[1.0] ~current:[2.0] = false

let%test "is_regression same distribution" =
  is_regression
    ~baseline:[1.0; 2.0; 3.0; 4.0; 5.0]
    ~current:[1.0; 2.0; 3.0; 4.0; 5.0] = false

let%test "is_regression significant increase" =
  is_regression
    ~baseline:[1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0]
    ~current:[5.0; 5.0; 5.0; 5.0; 5.0; 5.0; 5.0; 5.0; 5.0; 5.0] = true

let%test "is_regression slight increase not significant" =
  is_regression
    ~baseline:[10.0; 10.1; 9.9; 10.2; 9.8]
    ~current:[10.1; 10.2; 10.0; 10.3; 9.9] = false

let%test "effect_size too small" =
  effect_size [1.0] [2.0] = None

let%test "effect_size zero difference" =
  match effect_size [1.0; 2.0; 3.0] [1.0; 2.0; 3.0] with
  | Some d -> Float.abs d < 0.01
  | None -> false

let%test "effect_size large difference" =
  match effect_size
    [1.0; 1.0; 1.0; 1.0; 2.0]
    [5.0; 5.0; 5.0; 5.0; 4.0] with
  | Some d -> d > 3.0  (* Very large effect *)
  | None -> false

let%test "effect_size identical values returns None" =
  (* All same -> pooled_sd = 0 -> None *)
  effect_size [1.0; 1.0; 1.0] [1.0; 1.0; 1.0] = None
