open SortedFol
open PolicySyntax

module SMap = StringUtil.StringMap
module SSet = StringUtil.StringSet

(* FIRST PASS: check the policy against the vocab and against general
               restrictions - this is implemented in PolicyChecker.ml
   SECOND PASS: determine which rules nullify which others
   THIRD PASS:
   Part 1: compile the rules to "matches" formulas
   Part 2: use the graph of which rules nullify which to create the "applies"
           formulas
   FOURTH PASS: create each decision formula as the disjunction of its
                associated rules "applies" formulas *)
   

(* SECOND PASS: The empty structure is a map from each rule to the empty set.
                As overriding rules are discovered, they are added to the
                set to which each rule maps.
                If a rule overrides itself, then the overrides declarations are 
                in error.

                Create a list of the rule names (with their associated
                decisions) in the same order that they appear in the file. Since
                the parsing preserves the order of the rules, the list can be
                extracted from that. 
                This list will be used for FA declarations.
                
                Create a map from decisions to all rules associated with that
                decsion. This list will be used for Override declarations.
                
                For each Overrides rule combinator (Overrides a b) add
                all of the rules associated with decision b to each rule
                associated with a's override set. 
                For each FA rule combinator (FA a b) for each rule r associated
                with a, add all of the rules associated with b that come
                before r in the file to r's override set.*)

(** Module for a datatype for storing which rules override which other rules. *)
module Overs =
  struct
    type t = SSet.t SMap.t

    let empty : PolicySyntax.policy -> t =
      fun p ->
        let rules = List.map (fun r -> r.rule_name) p.rules in
        List.fold_left (fun m r -> SMap.add r SSet.empty m)
                       SMap.empty rules

    let add : t -> string -> string -> t =
      fun m r over -> 
        let updated = SSet.add over (SMap.find r m) in
        SMap.add r updated m
    
    let find = SMap.find
  end


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

