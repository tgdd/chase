open SortedFolBasic

let _ =
  let thy = string2theory "(exists y A (y = c)) (forall b B (not (r b)))" in
  let fml = string2formula "(and (y = c) (or (r y) (r c)))" in
  Syntax.output_formulas stdout thy;
  print_endline "";
  Syntax.output_formulas stdout [fml];
  print_endline "" 
