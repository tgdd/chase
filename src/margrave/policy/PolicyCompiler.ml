open SortedFol.Syntax
open PolicySyntax

open Overrides

(*
(** Takes a policy, list of decisions, and a particular rule and finds
    all rules with those decisions that come before the specified rule in
    the policy. *)
let get_earlier_rules_with_decisions : policy -> string list -> rule -> rule list =
  fun p ds r ->
    let earlier_rules = get_earlier_rules p r in
    let rules_with_decisions = decisions2rules p ds r in
    SSet.inter (list2string_set earlier_rules)
               (list2string_set rules_with_decisions) *)
    


(** For every decision in the list, take every associated rule, and 
let add_fa_nulls : rule list -> Nulls.t -> string list -> Nulls.t =
  fun rules nulls fa *)
(*
(** Adds all of the nullifications due to FA declarations to nulls. *)
let fa_nulls rules fas nulls =
  List.fold_left (add_fa_nulls rules) nulls fas
  

let get_nullifies : policy -> Nulls.t =
  fun p ->
    let fa_nulls = get_fa_nulls p.rules p.rule_combs.rule_fas Nulls.empty in
    get_over_nulls p.rules p.rule_combs.rule_overrides fa_nulls
    *)


(*(** Gets all rules that are higher priority than the specified rule in the
    given policy. Higher priority rules than a particular rule r include those
    that are specified as overriding r as well as those that both appear before
    r in the policy and appear in an first applicable clause with r. *)
let get_higher_priority pol rule =
  (* For getting those that are higher priority because they are FA.
     The intersection of fas and come_first take care of the FAs. *)
  let come_first = get_rules_defined_first pol rule in
  let fas = get_first_applicables pol rule in
  (* The rules that are higher priority because of overrides clauses. *)
  let overs = get_overriding_rules pol rule in
  SSet.union overs (SSet.inter come_first fas)

let matches_pred_name rule = rule.rule_name ^ "_{matches}"
let applies_pred_name rule = rule.rule_name ^ "_{applies}"
let rule_param_terms rule = List.map (fun v -> Var(v)) rule.rule_parameters





 let applies_implies_matches rule =
  let params = rule_param_terms rule in
  let applies = Pred(applies_pred_name rule, params) in
  let matches = Pred(matches_pred_name rule, params) in
  Implies(applies, matches)

(******************************************************************************)
(* APPLIES IMPLIES DECISION *)
let rule2applies_imp_descision rule =
  let params = rule_param_terms rule in
  let applies = Pred(applies_pred_name rule, params) in
  let decision = Pred(rule.rule_decision, params) in
  Implies(applies, decision)

let policy2applies_imp_descision pol =
  List.map rule2applies_imp_descision pol.rules
(* END APPLIES IMPLIES DECISION *)
(******************************************************************************)

(******************************************************************************)
(* APPLIES IMPLIES MATCHES *)
let rule2applies_imp_matches rule =
  let params = rule_param_terms rule in
  let applies = Pred(applies_pred_name rule, params) in
  let decision = Pred(matches_pred_name rule, params) in
  Implies(applies, decision)

let policy2applies_imp_matches pol =
  List.map rule2applies_imp_descision pol.rules
(* END APPLIES IMPLIES MATCHES *)
(******************************************************************************)

(******************************************************************************)
(* MATCHES IFF (TARGET AND RULE BODY) *)
let rule2matches_iff_body target rule =
  let body = And([target;rule.rule_body]) in
  let head = Pred(matches_pred_name rule, rule_param_terms rule) in
  Iff(body, head)

let policy2matches_iff_body pol =
  let r2f = rule2formula p.target in
  rule_result = List.map r2f p.rules
(* END MATCHES IFF (TARGET AND RULE BODY) *)
(***************************************************************************) *)

let augment_signature sgn p = sgn

(******************************************************************************)
(* COMPILER *)

(* The following has to wait until we fully define how the rule combinators are
   going to work.
(** Convert the policy to a sorted FOL theory.
    Additionally, for each rule r, there are four formulas:
    - r_matches iff the conjunction of the policy target and r.body.
    - If r_matches holds then either the r_applies holds
      or one of the higher priority rules' "applies" predicates holds.
    - r_applies holds only if r_matches holds.
    - r_applies implies r.desicion
    Additionally, for each decision d there is a formula stating
    that if the decision holds then for one of the associated rules, the
    "applies" predicate must hold. *)
let policy2theory sgn p =
  Policy.Checker.check_policy sgn policy;
  let m_iff_body = policy2matches_iff_body policy in (**)
  let m_imp_ao = policy2matches_implies_appilies pol in 
  let a_imp_m = policy2applies_imp_matches pol in (**)
  let a_imp_d = policy2applies_imp_descision pol in (**)
  let d_imp_a = policy2decision_imp_applies pol in 
  (m_iff_body @ m_imp_ao @ a_imp_m @ a_imp_d @ d_imp_a,
  augment_signature sgn pol) *)

let rule2head_iff_body t r =
  let params = List.map (fun v -> Var(v)) r.rule_parameters in
  let body = And([t;r.rule_body]) in
  let head = Pred(r.rule_decision, params) in
  Implies(body, head)

let policy2theory sgn p =
  PolicyChecker.check_policy sgn p;
  let thy = List.map (rule2head_iff_body p.target) p.rules in
  (sgn, thy)
(* END COMPILER *)
(******************************************************************************)

