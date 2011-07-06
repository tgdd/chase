open SortedFol

let _ =
  let thy = string2theory "(exists y A (y = c)) (forall b B (not (r b)))" in
  Syntax.output_formulas stdout thy;
  print_endline "" 
