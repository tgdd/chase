open Fol
open PolicySyntax

let rule_head : rule -> formula =
  fun r ->
    Pred(r.rule_decision, List.map (fun p -> Var(p)) r.rule_parameters)

let compile_policy : policy -> theory =
  fun p ->
    let target = SortedFol.sorted2unsorted p.target in
    List.map (fun r -> Implies(And([target
                                   ; SortedFol.sorted2unsorted r.rule_body]),
                               rule_head r))
             p.rules

