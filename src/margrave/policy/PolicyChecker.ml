(** For checking policies for consistency with respect to a signature
    and for internal consistency. For example, for checking the formulas for
    well-sortedness.

    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

open PolicySyntax
open SortedFol.Syntax

exception Policy_check_failure of string

module StringSet = StringUtil.StringSet

(******************************************************************************)
(* HELPERS *)
let list2string_set = StringUtil.list2string_set

(** Gets all intensional predicates from a policy. *)
let intensionals : policy -> pred_t list =
  fun p ->
    List.map (fun r -> r.rule_decision) p.rules

(** Gets all rule names form a policy. *)
let rule_names : policy -> string list =
  fun p ->
    List.map (fun r -> r.rule_name) p.rules

(* Gets the environment containing the allowed free variables. *)
let get_env : policy -> (var_t * sort_t) list =
  fun pol ->
    List.map (fun v -> (v.variable_name, v.variable_sort)) pol.vars

(** Gets the list of allowed free variable names. *)
let get_free_var_names : policy -> var_t list =
  fun pol ->
    List.map (fun v -> v.variable_name) pol.vars

let rule2basic_fmla : rule -> formula =
  fun r ->
    let params = List.map (fun v -> Var(v)) r.rule_parameters in
    let head = Pred(r.rule_decision, params) in
    Implies(r.rule_body, head)
(* END HELPERS *)
(******************************************************************************)


(******************************************************************************)
(* CHECK VARS *)
(** Check for duplicate variable names. *)
let check_vars_duplicates : policy -> unit =
  fun pol ->
    let names = List.map (fun v -> v.variable_name) pol.vars in
    let dup = ListUtil.find_duplicate names in
    match dup with
    | None -> ()
    | Some v ->
      raise (Policy_check_failure("Variable " ^ v ^ " appears multiple " ^ 
                                    "times in free variable declarations."))

(** Check to make sure all sorts exist in the signature. *)
let check_vars_sorts : sig_t -> policy -> unit =
  fun sgn pol ->
    let sorts = List.map (fun v -> v.variable_sort) pol.vars in
    try
      let missing = List.find (fun s -> not (Signature.mem_sort sgn s)) sorts in
      raise (Policy_check_failure("Sort " ^ missing ^ " used in free " ^
                                    "variable declarations, but not defined " ^
                                    "in vocabulary."))
    with Not_found -> ()
    

(** Makes sure that no varaible names are declared twice and all sorts
    appear in the signature. *)
let check_vars : sig_t -> policy -> unit =
  fun sgn pol ->
    check_vars_duplicates pol;
    check_vars_sorts sgn pol
(* END CHECK VARS *)
(******************************************************************************)


(******************************************************************************)
(* CHECK TARGET *)
(** Makes sure the target is well formed and well sorted. *)
let check_target : sig_t -> policy -> unit =
  fun sgn pol ->
    let env = get_env pol in
    let free_var_names = get_free_var_names pol in
    try
      (* Make sure the target is well formed. *)
      check_signature_violations sgn free_var_names pol.target;
      (* Make sure the target is well sorted. *)
      check_sort_violations sgn env pol.target
    with Signature_violation msg ->
      raise (Policy_check_failure("Problem with target: " ^ msg))
(* END CHECK TARGET *)
(******************************************************************************)


(******************************************************************************)
(* CHECK RULES *)
(** Makes sure there are no duplicate rule names. *)
let check_rules_names pol =
  let dup = ListUtil.find_duplicate (rule_names pol) in
  match dup with
  | None -> ()
  | Some d -> raise (Policy_check_failure("The rule name " ^ d ^
                                            " is used multiple times."))

(** Make sure an individual rule is well-formed. *)
let check_rule_form sgn free_var_names rule =
  let fmla = rule2basic_fmla rule in
  try check_signature_violations sgn free_var_names fmla
  with Signature_violation m ->
    raise (Policy_check_failure("Problem with rule " ^ rule.rule_name ^
                                  ": " ^ m))

(** Makes sure the rules are well-formed. *)
let check_rules_forms sgn pol =
  let free_var_names = get_free_var_names pol in
  List.iter (check_rule_form sgn free_var_names) pol.rules

(** Makes sure an individual rule is well-sorted. *)
let check_rule_sorts sgn env rule =
  let fmla = rule2basic_fmla rule in
  try check_sort_violations sgn env fmla
  with Signature_violation m ->
    raise (Policy_check_failure("Problem with rule " ^ rule.rule_name ^
                                  ": " ^ m))

(** Makes sure the rules are well-sorted. *)
let check_rules_sorts sgn pol =
  let env = get_env pol in
  List.iter (check_rule_sorts sgn env) pol.rules

(** Makes sure the rules have no duplicate names, are well formed, and
    are well-sorted *)
let check_rules : sig_t -> policy -> unit =
  fun sgn pol ->
    check_rules_names pol;
    check_rules_forms sgn pol;
    check_rules_sorts sgn pol
(* END CHECK RULES *)
(******************************************************************************)


(******************************************************************************)
(* CHECK RCOMBS *)

(** Gets then names used in an override declaration. *)
let override_names_used override =
  override.low_rule :: override.high_rules 

(** Get the names used for FAs and Overrides declarations. *)
let rcomb_names_used rcombs =
  let fa_names = List.concat rcombs.rule_fas in
  let override_names = ListUtil.map_append override_names_used rcombs.rule_overrides in
  list2string_set (fa_names @ override_names)

(** Makes sure the rule combinator predicates actually appear as intensional
    predicates. *)
let check_rcombs_names pol =
  let names = list2string_set (intensionals pol) in
  let rcomb_names = rcomb_names_used pol.rule_combs in
  let diffs = StringSet.diff rcomb_names names in
  if StringSet.is_empty diffs then () else
  let names = StringSet.fold (fun name names -> name ^ " " ^ names) diffs "" in
  raise (Policy_check_failure("The following decisions are refered to in the " ^
                              "rule combinator definitions, but are not " ^
                              "used: " ^ names))

(** Make sure that rule combinators predicates appear as intensional
    predicates. *)
let check_rcombs : policy -> unit =
  fun pol ->
    check_rcombs_names pol
(* END CHECK RCOMBS *)
(******************************************************************************)


(******************************************************************************)
(* CHECK POLICY *)
(** Ensures a policy is well-formed w.r.t. a signature. Raises
    Policy_check_failure if there is an error. *)
let check_policy : sig_t -> policy -> unit =
  fun sgn pol ->
    check_vars sgn pol;
    check_target sgn pol;
    check_rules sgn pol;
    check_rcombs pol
(* END CHECK POLICY *)
(******************************************************************************)
