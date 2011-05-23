(* Time-stamp: "Thu 7/15/10 11:47 Dan Dougherty chase.ml"  *)
(* I think this works ! *)
(*

  Notes 

- OK to enter a simple eca formula as a rule, will be coerced to have
  True as antecedent

  - Caution!  We HAVE TO DO THAT!
   True and false are not recognized as concrete syntax, so 
  holds empty_model <<True ==> False>> is true, since 
  <<True ==> False>> is just an implication between two uniniterpretd preds !
    holds empty_model (Imp(True, False));;

   - in satisfy_eca_formula: the integer modelsize(m) is the first int not in m 
  What if Barendregt convention is not followed?? Does satisfy_eca_formula break? 

  - in a rule,  an empty LHS is the empty conjunction: true.
  an empty RHS is the empty disjunction: false.
  the RHS which is [[]] is true!

  - a  typical rule/sequent:

  ( [z;w] , << exists x y . P(x,y) & Q(z,x) >> , [<<W(w)>>; <<exists x . Q(x,w)] ) 

  TODO: 

  - allow one-sided sequents, by introducing tt and ff as constants in
    the concrete syntax.

  - this version returns a fact list (rather than a real model...)  [TRUE?]
  - fmlate and prove the invariants
  - implement iterative deepening, for non-terminating cases
  - look for optimiztions to find answer models in non-terminating cases
  - use a better data structure, eg tag models with the (rule,env) pairs know to hold
  - print models better!

LEFTOVER Alternate forms

let rule_variables ((x,y,z):rule) = x and
    rule_antecedent ((x,y,z):rule) = y and
    rule_consequent ((x,y,z):rule) = z;;

let (rule_to_formula : rule -> folFormula) = 
  fun r -> Imp( (rule_antecedent r), 
                (list_to_disjunct (rule_consequent r)));;

 let empty_consequent r = (List.length (rule_consequent r)) = 0;; 
            
let all_env_rule_pairs_for_rule (m: model) (r: rule) =
  let envs =  (allenvs (rule_variables r) m)
  in List.map (fun e -> (r,e)) envs;;
*)

 (* OUTLINE :

    Each (rule,env) pair determines a function: model -> modelSet,
trading a model for a set of models that each satisfy the rule under the
environment.  So the lift of that gives a function modelSet -> modelSet.
If we compose these across all the environments we get a set of models
that each satisfy the rule under all the given environments. *)

(* ======================== *)


#use "basics.ml";;

(* CAUTION!  here we treat lists as sets -- must be ordered  *)
(* How to enforce that invariant?? *)
type modelSet = model list;;

type ecaFormula =  folFormula;; (* should ONLY be an exist'lly quantified conjunction of atomics *)
type ruleLHS = ecaFormula;;  
type ruleRHS = ecaFormula list;;   
type rule = variable list * ruleLHS * ruleRHS;;
type thy = folFormula list;;    (* should ONLY be geometric formulas *)


(* ==========================  *)
(*  Geometric logic utilities  *)
(* ==========================  *)

(* Parsing formulas as rules *)
(* Assume universal quantification is NOT given *)
(* no error-checking here !! *)
let rec (formula_to_rule : folFormula -> rule) = 
    fun f ->
      match f with 
          Imp(p,q) -> ((fv f), p, (disjuncts q))
        | Exists(_,_) -> formula_to_rule (Imp(True,f))
        |_ -> failwith "bad formula in formula_to_rule" ;;

(* let rec list_to_conjunct l = match l with *)
(*     []  -> True *)
(*   | [x] -> x *)
(*   | x::xs -> And(x, (list_to_conjunct xs));; *)

let rec list_to_disjunct l = match l with
    []  -> False
  | [x] -> x
  | x::xs -> Or(x, (list_to_disjunct xs));;

let rule_to_formula  ((vs,lhs,rhs):rule) =
  Imp( lhs, (list_to_disjunct rhs));;

let empty_consequent ((vs,lhs,rhs):rule) = (List.length rhs) = 0;;

let all_env_rule_pairs_for_rule (m: model)  ((vs,lhs,rhs) as r:rule) = 
  let envs =  (allenvs vs m)
  in List.map (fun e -> (r,e)) envs;;

(* all the (rule,env) pairs appropriate to model m *)
let all_rule_env_pairs (m:model)  (rs: rule list)  =
  List.flatten (List.map  (all_env_rule_pairs_for_rule m) rs);;

(* model -> rule * environment -> bool *)
let rule_env_holds m (r,e) = holdsenv (rule_to_formula r) m e;;

(* pick m rs returns Some (r,e) where m doesn't satisfy r under e, or None if no such pair *)
(* model -> rule list -> (rule * environment) option *)
let pick_rule_env (m:model) (rs: rule list)  =
  let pairs = all_rule_env_pairs m rs in
    try Some List.find (fun p -> not(rule_env_holds m p)) pairs
    with Not_found -> None;;

                         (* ================ *)
                         (* The chase proper *)
                         (* ================ *)

type chaseResult = Success | NotYet of model list;;

(* The update making an ecaFormula true on m---no change if already true. *)
(*   model -> environment -> ecaFormula -> model *)
let rec satisfy_eca_formula (m: model) (e:environment) (f:ecaFormula) = 
  match f with
      Exists(x,g) -> satisfy_eca_formula (extend_domain m) (env_update e x (modelsize(m))) g
    | _ -> let new_facts = List.map (make_fact e) (conjuncts f)
      in (extend_facts m new_facts);;

(* if rule holds return singleton; else return set of updated models,
which might be empty! -- if the rule RHS is empty *)
(*  rule -> model -> environment -> modelSet *)
let apply_rule_env  ((vs,lhs,rhs) as r:rule)  (e:environment) (m:model)  = 
  if (holdsenv (rule_to_formula r) m e) then ([m] : modelSet)
  else image (satisfy_eca_formula m e) rhs;;


(* Returns Success if m is a model of rs, otherwise
NotYet (a list of m') where each m' is an update of m *)
(*  rule list -> model -> chaseResult *)
let handle_model (rs: rule list) (m: model) =
  match (pick_rule_env m rs) with
      None -> Success
    | Some (r,e) -> NotYet (apply_rule_env r e m);;


(*  thy * (model list * model list) -> model list *)
let rec the_loop ((rs: rule list), ((models_of_t, pending): (model list) * (model list))) = 
  match pending with
      [] -> models_of_t
    | (p::ps) ->
        match (handle_model rs p) with
            Success -> the_loop (rs , ((p :: models_of_t) , ps) )
          | NotYet ms -> the_loop (rs, (models_of_t, ps@ms) );;

(* chase: thy -> model list *)
let chase (t : thy) =
  let rules = List.map formula_to_rule t 
  and start_state = ([], [empty_model])
  in the_loop (rules, start_state) ;;

let show_chase (t:thy)  = show_models (chase t);;  

(* Testing *)

let go =   show_chase;;

let em = empty_model;;
let m1 = extend_domain em;;
let m2 = extend_domain m1;;

let g0 = <<exists x y. R(x,y)>>;;
let g1 = <<R(x,w) ==> exists y . Q(x,y)>>;;
let g2 = << Quv ==>exists z . Ruz>>;;
let t0 = [g0];;
let t1 = [g0;g1];;
let t2 = [g0;g1;g2];;
 
go t0;;
go t1;;
go t2;;



