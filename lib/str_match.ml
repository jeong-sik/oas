let contains re str =
  try ignore (Str.search_forward re str 0); true
  with Not_found -> false
