open SortedFol
open PolicySyntax

let rule_head : rule -> formula =
  fun r ->
    Pred(r.rule_decision, List.map (fun p -> Var(p)) r.rule_parameters)

let compile_policy : policy -> theory =
  fun p ->
    List.map (fun r -> Implies(And([p.target; r.rule_body]), rule_head r))
             p.rules

