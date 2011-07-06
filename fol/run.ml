open Fol

let _ =
  let thy = string2theory "(exists y A (y = c))" in
  Syntax.output_formulas stdout thy
