(* Time-stamp: "Wed 6/9/10 13:04 Dan Dougherty basics.ml"  *)



(* ------------------------------------------------------------------------- *)
(* Type definitions for FOL *)
(* ------------------------------------------------------------------------- *)
type variable = string;;

type term = Var of variable
          | Fn of string * term list;;

type atomicFormula = string * term list;;

type folFormula = False
                 | True
                 | Atom of atomicFormula
                 | Not of folFormula
                 | And of folFormula * folFormula
                 | Or of folFormula * folFormula
                 | Imp of folFormula * folFormula
                 | Iff of folFormula * folFormula
                 | Forall of string * folFormula
                 | Exists of string * folFormula;;


(* ------------------------------------------------------------------------- *)
(* Parsing and printing                                                      *)
(* ------------------------------------------------------------------------- *)
#use "fol-parse-print.ml";;

(* ------------------------------------------------------------------------- *)
(* Syntax                                                                    *)
(* ------------------------------------------------------------------------- *)


(* OCaml won't let us use the constructors as functions.                     *)
let mk_and p q = And(p,q) 
and mk_or p q = Or(p,q)
and mk_imp p q = Imp(p,q) 
and mk_iff p q = Iff(p,q)
and mk_forall x p = Forall(x,p) 
and mk_exists x p = Exists(x,p);;

(* ------------------------------------------------------------------------- *)
(* Destructors.                                                              *)
(* ------------------------------------------------------------------------- *)

let dest_iff fm = match fm with Iff(p,q) -> (p,q) | _ -> failwith "dest_iff";;

let dest_and fm = match fm with And(p,q) -> (p,q) | _ -> failwith "dest_and";;

let rec conjuncts fm = 
  match fm with And(p,q) -> conjuncts p @ conjuncts q | _ -> [fm];;

let dest_or fm = match fm with Or(p,q) -> (p,q) | _ -> failwith "dest_or";;

let rec disjuncts fm = 
  match fm with Or(p,q) -> disjuncts p @ disjuncts q | _ -> [fm];;

let dest_imp fm = match fm with Imp(p,q) -> (p,q) | _ -> failwith "dest_imp";;

let antecedent fm = fst(dest_imp fm);;
let consequent fm = snd(dest_imp fm);;

(* ------------------------------------------------------------------------- *)
(* Apply a function to the atoms, otherwise keeping structure.               *)
(* ------------------------------------------------------------------------- *)

let rec onatoms f fm =
  match fm with
      Atom a -> f a
    | Not(p) -> Not(onatoms f p)
    | And(p,q) -> And(onatoms f p,onatoms f q)
    | Or(p,q) -> Or(onatoms f p,onatoms f q)
    | Imp(p,q) -> Imp(onatoms f p,onatoms f q)
    | Iff(p,q) -> Iff(onatoms f p,onatoms f q)
    | Forall(x,p) -> Forall(x,onatoms f p)
    | Exists(x,p) -> Exists(x,onatoms f p)
    | _ -> fm;;

(* ------------------------------------------------------------------------- *)
(* Formula analog of list iterator "itlist".                                 *)
(* ------------------------------------------------------------------------- *)

let rec overatoms f fm b =
  match fm with
      Atom(a) -> f a b
    | Not(p) -> overatoms f p b
    | And(p,q) | Or(p,q) | Imp(p,q) | Iff(p,q) ->
        overatoms f p (overatoms f q b)
    | Forall(x,p) | Exists(x,p) -> overatoms f p b
    | _ -> b;;

(* Special case of a union of the results of a function over the atoms.      *)
let atom_union f fm = setify (overatoms (fun h t -> f(h)@t) fm []);;

(* Special case of applying a subfunction to the top *terms*.                *)
let onformula f = onatoms(fun ((p,a)) -> Atom((p,map f a)));;

(* ------------------------------------------------------------------------- *)
(* Free variables in terms and formulas.                                     *)
(* ------------------------------------------------------------------------- *)

let rec fvt tm =
  match tm with
      Var x -> [x]
    | Fn(f,args) -> unions (map fvt args);;

let rec var fm =
  match fm with
      False | True -> []
    | Atom(p,args) -> unions (map fvt args)
    | Not(p) -> var p
    | And(p,q) | Or(p,q) | Imp(p,q) | Iff(p,q) -> union (var p) (var q)
    | Forall(x,p) | Exists(x,p) -> insert x (var p);;

let rec fv fm =
  match fm with
      False | True -> []
    | Atom(p,args) -> unions (map fvt args)
    | Not(p) -> fv p
    | And(p,q) | Or(p,q) | Imp(p,q) | Iff(p,q) -> union (fv p) (fv q)
    | Forall(x,p) | Exists(x,p) -> subtract (fv p) [x];;

(* ------------------------------------------------------------------------- *)
(* Universal closure of a formula.                                           *)
(* ------------------------------------------------------------------------- *)

let generalize fm = itlist mk_forall (fv fm) fm;;

(* ------------------------------------------------------------------------- *)
(* Substitution within terms.                                                *)
(* ------------------------------------------------------------------------- *)

let rec tsubst sfn tm =
  match tm with
      Var x -> tryapplyd sfn x tm
    | Fn(f,args) -> Fn(f,map (tsubst sfn) args);;

(* ------------------------------------------------------------------------- *)
(* Variant function                                                          *)
(* ------------------------------------------------------------------------- *)

let rec variant x vars =
  if mem x vars then variant (x^"'") vars else x;;

(* ------------------------------------------------------------------------- *)
(* Substitution in formulas, with variable renaming.                         *)
(* ------------------------------------------------------------------------- *)

let rec subst subfn fm =
  match fm with
      False -> False
    | True -> True
    | Atom(p,args) -> Atom(p,map (tsubst subfn) args)
    | Not(p) -> Not(subst subfn p)
    | And(p,q) -> And(subst subfn p,subst subfn q)
    | Or(p,q) -> Or(subst subfn p,subst subfn q)
    | Imp(p,q) -> Imp(subst subfn p,subst subfn q)
    | Iff(p,q) -> Iff(subst subfn p,subst subfn q)
    | Forall(x,p) -> substq subfn mk_forall x p
    | Exists(x,p) -> substq subfn mk_exists x p

and substq subfn quant x p =
  let x' = if exists (fun y -> mem x (fvt(tryapplyd subfn y (Var y))))
    (subtract (fv p) [x])
  then variant x (fv(subst (undefine x subfn) p)) else x in
    quant x' (subst ((x |-> Var x') subfn) p);;

(* Above stuff is essentially from Harrison *)



(* ========= *)
(* Semantics *)
(* ========= *)


(*
First-order logic: models
each domain is (0--n) some some n
relations only
each relation interpreted as a set of "facts"
a fact is a (string * (int list)) pair

 Note.  When formulas are parsed, numbers are treated as logical
   constants in that they are parsed as Fn(f,[]) where f will be the
   string naming the number.  This is controlled by the fuction
   is_const_name : string -> bool.  Then models (below) interpret these
   constants using int_of_string(f).
   ----------------------------------------------------------------------- *)

(* using "string" here is clumsy -- change to using a type of "relation_name".
But when do so -- re-do the type of atomic formulas as well *)
type fact = string * (int list);;

module Fact_set = Set.Make(
  struct type t = fact let compare = Pervasives.compare end);;

type factSet = Fact_set.t;;

(* shorthand *)
type model = (int list) * factSet;;

let (empty_model: model) = ( [], Fact_set.empty) ;;

let rec factlist_to_Fact_set (l) =
  match l with 
      [] -> Fact_set.empty
    | f::fs -> (Fact_set.add f (factlist_to_Fact_set fs));;


let make_model n factlist =  (0--(n-1), (factlist_to_Fact_set factlist));;

let domain_of (m: model)  = (fst m) ;;
let facts_of (m: model) = (snd m);;
let modelsize ((domain,fs):model) = length(domain);;


let addfactSet  ((domain,fs):model) (newfs:(factSet)) =
  (domain, (Fact_set.union newfs fs));;

(* expand the domain *)
let addelts  ((domain,fs):model) (n:int) =
  let l = length(domain) in (0--(l+n), fs);;

(* Add a new elt to domain of m *)
let extend_domain (m:model) = (0--(modelsize(m)), (facts_of m));;


let rec add_factlist_to_factset l s = 
  match l with
      [] -> s
    | x::xs -> (Fact_set.add x (add_factlist_to_factset xs s));;

(* Add a new set of facts to m, presented as a list of facts *)
let extend_facts (m:model) (l: fact list) = 
  ( domain_of(m), add_factlist_to_factset l (facts_of m));;



(* ------------ *)
(* Environments *)
(* ------------ *)


(* envs as association lists *)
(** FIXME -- lookups should check for terms undefined under v **)

type environment = (string * int) list;;  
let env_lookup e  (x: string) =  assoc x e ;; 
let env_update e (x:string) value = (x,value )::e ;; 
let empty_env = [];; 
let domain_env e = map (fun (x,y) -> x) e;;
let in_domain x e = List.mem_assoc x e;;


(* ========================== *)
(* Generating all envronments *)
(* ========================== *)

(* Here l is a list of lists, we cons item onto each *)
let mapcons item l =
  map (fun x -> item::x) l ;;

(* Here l is list, we add pair item with each elt of l *)
let mappair item l =
  map (fun x -> (item, x)) l ;;

(* the "product" of two lists, where the operator is cons *)
(* input [a1;a2;..]  [l1;l2;...] *)
(* output [ a1::l1; a1::l2; ... a2::l1;  a2::l2; .. ] *)
let rec prodlist l1 l2 = 
  match l1 with 
      [] -> []
    | h::t -> 
        (mapcons h l2) @ (prodlist t l2) ;;

(* All mappings as (association lists) between dom and ran *)
(* Harrison's code had first two lines swapped, which is wrong,
since ([],[]) should -> [[]]. Fixed here. *)
let rec allmaps dom ran =
  match (dom,ran) with 
    ([], ran) -> [[]]
    | (dom, []) ->  []
    | (h1::t1, ran) -> 
        let result1 = allmaps t1 ran 
        and tmp = mappair h1 ran
        in prodlist tmp result1 ;;

let allenvs (vs: string list) (m:model) =
  ((allmaps vs (domain_of m)): environment list) ;;



(* Interpretation of a term.  
   Via the parsing, constants are ints-as-strings.
   We allow no other function symbols
*)
let rec terval (v:environment) (tm:term) =
  match tm with
      Var(x) -> env_lookup v x
    | Fn(f,[]) -> int_of_string f 
    | Fn(f,args) -> failwith "no functions allowed" ;;


(* These depend on the fact that model domains are lists *)
let model_foreach (m:model) p =   List.for_all p (domain_of m);;
let model_forsome (m:model) p =   List.exists p (domain_of m);;

(* make a fact from am atomic formula and an environment *)
let make_fact (e:environment) (f: folFormula) =  
  match f with
      Atom(r, args) -> ((r,  (map (terval e) args)): fact)
    | _ -> failwith "bad input to make_fact";; 

(* Truth of a fmla in a model *)
let rec holdsenv (fm: folFormula) ((domain,fs as m):model) (e:environment) = 
  match fm with
      False -> false
    | True -> true
    | Atom("=",[x;y]) -> ((terval e x) = (terval e y))
    | Atom(r,args) -> Fact_set.mem (r, (map (terval e) args)) fs 
(*    | Atom(r,args) -> Fact_set.mem  (make_fact e fm) fs *)
    | Not(p) -> not (holdsenv p m e)
    | And(p,q) -> (holdsenv p m e) & (holdsenv q m e)
    | Or(p,q) -> (holdsenv p m e) or (holdsenv q m e)
    | Imp(p,q) -> (not(holdsenv p m e)) or (holdsenv q m e)
    | Iff(p,q) -> (holdsenv p m e = holdsenv q m e )
    | Forall(x,p) -> model_foreach m (fun a -> holdsenv p m (env_update e x a))
    | Exists(x,p) -> model_forsome m (fun a -> holdsenv p m (env_update e x a)) ;;

(* Truth of a sentence in a model *)
let holds m fm = holdsenv fm m empty_env;;


(*
  ========
  Printing models 
  ======== 
*)


(* Just the facts (!) *)
let show_model_facts  ((domain,fs):model) =
 (Fact_set.elements(fs) : fact list);;

let show_model m = ((domain_of m), (show_model_facts m));;

let show_models = List.map show_model;;

(* testing *)
let mm1 = make_model 3     
  [ ("P", [0]) ; ("P", [1]) ; ("P", [2]); ("R", [0;1]); ("R", [1;1]); ];;
show_model mm1;;


(* Here p is a printing function, left and right are the enclosing "brackets",
   and delim is the string to be printed between list items, 
   including any desired spaces *)
let print_list p left right delim ls = 
  let rec helper ls = 
    match ls with
        [] -> ()
      | [x] -> p(x)
      | h::t -> p(h); print_string(delim);  helper t
  in   print_string(left);
    helper ls;
    print_string(right) ;;

(* t_s is the to_string fuction for the element, left, right and delim are strings *)
let list_to_string t_s left right delim ls =
  let rec helper ls = 
    match ls with
        [] -> failwith "empty list in list_to_string helper"
      | [x] -> t_s(x)
      | h::t -> (t_s(h))^delim^(helper t)
  in  left^(helper ls)^right;;

let fact_to_string ((name,args): fact) = 
  name^(list_to_string string_of_int "(" "," ")" args);;

let print_fact ((name,args): fact) = 
  print_string(name);
  print_list print_int "(" ")" ", " args ;;

let print_factSet (fs) = 
  print_list print_fact "[" "]" "; " (Fact_set.elements(fs)) ;;

let print_model ((domain, fs):model) =
  print_string("Length: "); print_int (length(domain));
  print_string "\n";
  print_factSet fs;;

