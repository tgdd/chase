open Batteries
open SortedFol.Syntax
module S = SortedFol.Signature.Signature
open PolicySyntax

let matches_pred rule = rule.rule_name ^ "_{matches}"
let applies_pred rule = rule.rule_name ^ "_{applies}"
let strings2vars = List.map (fun v -> Var v)

(******************************************************************************) 
(* Handles augmenting the signature with the matches and applies predicates. *)
let rec augment_signature' sgn rs = match rs with
  | [] -> sgn
  | (r::rs) ->
    let arity = (S.pred_arity sgn r.rule_decision) in
    let sgn = S.add_pred sgn (matches_pred r) arity in
    let sgn = S.add_pred sgn (applies_pred r) arity in
    augment_signature' sgn rs
  
let augment_signature sgn p =
  augment_signature' sgn p.rules
(* END Handles augmenting the signature with the matches and applies predicates. *)
(******************************************************************************)

(******************************************************************************)
(* Handles the part of the theory that states when rules match. *)
let rule2head_iff_body t r =
  let params = List.map (fun v -> Var(v)) r.rule_parameters in
  let body = And([t;r.rule_body]) in
  let head = Atom(matches_pred r, params) in
  Iff(head, body)
let matches_thy p =
  List.map (rule2head_iff_body p.target) p.rules
(* END Handles the part of the theory that states when rules match. *)
(******************************************************************************)

(******************************************************************************)
(* Handles the part of the theory that states when rules apply. *)

(* for figuring out overrides *)
let get_ovs' dcrd = match dcrd with
  | Overrides(low, highs) -> List.map (fun h -> (low, h)) highs
  | _ -> []
let get_ovs p =
  List.flatten (List.map get_ovs' p.dcrds)

(* for figuring out FAs *)
let rule_before p lhs rhs =
  List.mem rhs (ListUtil.drop_until (fun r -> r = lhs) p.rules)

let get_fas' dcrd = match dcrd with
  | FirstApplicable ds -> ListUtil.cartesian_product ds ds
  | _ -> []

let get_fas p =
  List.unique (List.flatten (List.map get_fas' p.dcrds))

let is_overridden_by p =
  let ovs = get_ovs p in
  let fas = get_fas p in
  let before = rule_before p in
  fun low high ->
   ( List.mem (low.rule_decision, high.rule_decision) ovs
   || ( low <> high 
      && before high low
      && List.mem (low.rule_decision, high.rule_decision) fas
      )
   )

let find_suppressors p =
  let is_overridden_by = is_overridden_by p in
  fun r ->
    List.filter (is_overridden_by r) p.rules

(* converts suppression information into a formula *)
let suppression2fmla s =
  let low = fst s in
  let highs = snd s in
  let nots = List.map (fun high -> Atom(matches_pred high, strings2vars low.rule_parameters)) highs in
  Iff( Atom(applies_pred low, strings2vars low.rule_parameters)
     , And([ Atom(matches_pred low, strings2vars low.rule_parameters)
           ; Not (Or nots)]))

let applies_thy p =
  let find_suppressors = find_suppressors p in
  let suppressed_by = List.map (fun r -> (r, find_suppressors r)) p.rules in
  List.map suppression2fmla suppressed_by
  
(* END Handles the part of the theory that states when rules apply. *)
(******************************************************************************)

(******************************************************************************)
(* Handles the part of the theory that states when decisions hold. *)
let decision_params p d =
  let r = (List.find (fun r -> r.rule_decision = d) p.rules) in
  r.rule_parameters

let decision_rules p d =
  List.filter (fun r -> r.rule_decision = d) p.rules

let decision_thy p d =
  let params = strings2vars (decision_params p d) in
  let rules = List.map applies_pred (decision_rules p d) in
  Iff(Atom(d, params), Or(List.map (fun r -> Atom(r, params)) rules))

let decisions p =
  List.unique (List.map (fun r -> r.rule_decision) p.rules)

let decisions_thy p =
  let decs = decisions p in
  List.map (decision_thy p) decs
(* END Handles the part of the theory that states when decisions hold. *)
(******************************************************************************)

(******************************************************************************)
(* Puts it all together. *)
let compile_policy sgn p =
  (* PolicyChecker.check_policy sgn p; *)
  let matches = matches_thy p in
  let applies = applies_thy p in
  let decs = decisions_thy p in
  let sgn = augment_signature sgn p in
  (sgn, matches @ applies @ decs)
(* END Puts it all together *)
(******************************************************************************)

