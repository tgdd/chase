(** Module for sorted first order logic type definitions and utilities.

    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)
open Batteries
open SortedFolFormulas
open StringUtil
open ListUtil
open FunUtil

(******************************************************************************)
(* Types *)

exception Signature_violation of string

(* for ease of reading *)
type var_t = string
type sort_t = string
type pred_t = string
type func_t = string

(* actual data structures *)
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)
module StringGraph = Graph.Persistent.Digraph.Concrete(GraphUtil.StringVertex)
module StringGraphOper = Graph.Oper.P(StringGraph)

(* for ease of reading *)
module SG = StringGraph (* sort graph *)
module SGOper = StringGraphOper
module SGUtil = GraphUtil.Make(SG)
module PM = StringMap  (* pred map *)
module FM = StringMap (* func map *)

(* Signature *)
module Signature = struct
    type sig_t = { sorts : SG.t
             ; funcs : (sort_t list * sort_t) FM.t
             ; preds : sort_t list PM.t
             }
    let init sorts subsorts =
      { sorts =
          (let g = List.fold_left SG.add_vertex SG.empty sorts in
          let add_edge = fun g ss -> SG.add_edge g (fst ss) (snd ss) in
          let g = List.fold_left add_edge g subsorts in
          SGOper.transitive_closure ~reflexive:true g)
      ; funcs = FM.empty
      ; preds = PM.empty
      }
    
    let invalid_name f =
      invalid_arg ("Item with name " ^ f ^ " already in signature.")
    let invalid_sort s = 
      invalid_arg ("Sort " ^ s ^ " used, but does not appear in signature.")
    let check_sort sgn s =
      if SG.mem_vertex sgn.sorts s then () else invalid_sort s
    let check sgn name sorts =
      if FM.mem name sgn.funcs || PM.mem name sgn.preds
      then invalid_name name else
      List.iter (check_sort sgn) sorts
    
    let add_func sgn f arity res =
      check sgn f (res::arity);
      { sgn with funcs = FM.add f (arity, res) sgn.funcs }
    
    let add_pred sgn p arity =
      check sgn p arity;
      { sgn with preds = PM.add p arity sgn.preds }
    
    let mem_sort sgn s = SG.mem_vertex sgn.sorts s
    let is_subsort sgn sup sub = SG.mem_edge sgn.sorts sup sub
    let mem_func sgn f = FM.mem f sgn.funcs
    let func_rank sgn f = FM.find f sgn.funcs
    let mem_pred sgn p = PM.mem p sgn.preds
    let pred_arity sgn p = PM.find p sgn.preds

    let show sgn = "Sorts: "
                 ^ (SG.fold_vertex (fun s r -> s ^ "," ^ r) sgn.sorts "")
                 ^ "\n"
                 ^ "Subsorts: "
                 ^ (SG.fold_edges (fun a b r -> a ^ " < " ^ b ^ "; " ^ r)
                                  sgn.sorts "")
                 ^ "\n"
                 ^ "Preds: "
                 ^ (PM.fold (fun k v r -> k ^ " " ^ (StringUtil.unwords v)
                                          ^ "; " ^ r)
                            sgn.preds "")
                 ^ "\n"
                 ^ "Funcs: "
                 ^ (FM.fold (fun k v r -> k ^ " " 
                                          ^ (StringUtil.unwords (fst v))
                                          ^ " -> " ^ (snd v) ^ "; " ^ r)
                            sgn.funcs "")
    let equal a b = SGUtil.equal a.sorts b.sorts
                 && FM.equal (=) a.funcs b.funcs
                 && PM.equal (=) a.preds b.preds
  end
type sig_t = Signature.sig_t

(******************************************************************************)
(* Information about formulas *)

(* finding free vars *)
(* helper *)
let not_contains env term =
  if List.mem term env then [] else [term]

let rec term_free_vars env term = match term with 
  | Var(name) -> not_contains env name
  | FunApp(name, terms) -> map_append (term_free_vars env) terms

let rec free_vars' : var_t list -> formula -> var_t list =
  fun env fmla -> match fmla with
    | And(fs) -> map_append (free_vars' env) fs
    | Or(fs) -> map_append (free_vars' env) fs
    | Not(f) -> free_vars' env f
    | Implies(lhs,rhs) -> free_vars' env lhs @ free_vars' env rhs
    | Iff(lhs,rhs) -> free_vars' env lhs @ free_vars' env rhs
    | Exists(v,s,f) -> free_vars' (v::env) f
    | Forall(v,s,f) -> free_vars' (v::env) f
    | Equals(lhs,rhs) -> term_free_vars env lhs @ term_free_vars env rhs
    | Atom(name, terms) -> map_append (term_free_vars env) terms

let free_vars fmla = free_vars' [] fmla

let is_sentence fmla = List.is_empty (free_vars fmla)

type environment = (var_t * sort_t) list

let rec term_signature_violations : sig_t -> var_t list -> term -> string list =
  fun sgn env t -> match t with
    | Var(name) ->
      if List.mem name env then [] else
      ["The variable " ^ name ^ " was not bound and was not provided in " ^
       "the list of allowed free variables."]
    | FunApp(name, args) ->
      let viols = term_signature_violations sgn env in
      let msgs = map_append viols args in
      if Signature.mem_func sgn name then msgs else
      ("Function name " ^ name ^ " does not appear in the signature.") :: msgs

let rec signature_violations : sig_t
                            -> var_t list
                            -> formula
                            -> string list =
  fun sgn env fmla -> match fmla with
    | And(fs) -> map_append (signature_violations sgn env) fs
    | Or(fs) -> map_append (signature_violations sgn env) fs
    | Not(f) -> signature_violations sgn env f
    | Implies(lhs,rhs) ->
      let viols = signature_violations sgn env in
      viols lhs @ viols rhs
    | Iff(lhs,rhs) ->
      let viols = signature_violations sgn env in
      viols lhs @ viols rhs
    | Exists(v,s,f) ->
      let msgs = signature_violations sgn (v::env) f in
      if Signature.mem_sort sgn s then msgs else
      ("The sort name " ^ s ^ " does not appear in the signature.") :: msgs
    | Forall(v,s,f) ->
      let msgs = signature_violations sgn (v::env) f in
      if Signature.mem_sort sgn s then msgs else
      ("The sort name " ^ s ^ " does not appear in the signature.") :: msgs
    | Equals(lhs,rhs) ->
      let viols = term_signature_violations sgn env in
      viols lhs @ viols rhs
    | Atom(name, terms) ->
      let msgs = map_append (term_signature_violations sgn env) terms in
      if Signature.mem_pred sgn name then msgs else
      ("Predicate name " ^ name ^ " does not appear in the signature.") :: msgs

let meets_signature sgn env fmla = List.is_empty (signature_violations sgn env fmla)

let check_signature_violations sgn env fmla =
  let msgs = signature_violations sgn env fmla in
  match msgs with
  | m::_ -> raise (Signature_violation m)
  | [] -> ()

let rec is_subsort sgn sub sup = match sub, sup with
    | [], [] -> true
    | (x::xs), (y::ys) when Signature.is_subsort sgn x y ->
      is_subsort sgn xs ys
    | _ -> false

let rec term_sort_violations : sig_t -> environment -> term -> (string list * sort_t) =
  fun sgn env t -> match t with
    | Var(name) -> ([], List.assoc name env)
    | FunApp(name, args) -> 
      let res = List.map (term_sort_violations sgn env) args in
      let msgs = map_append fst res in
      let used_arity = List.map snd res in
      let expected_arity, ret_sort = Signature.func_rank sgn name in
      if is_subsort sgn expected_arity used_arity then (msgs, ret_sort)  else
      (("Function " ^ name ^ " expects arity (" ^ comma_delim expected_arity ^
       ") but was used with arity (" ^ comma_delim used_arity ^ ").") :: msgs,
       ret_sort)

let rec sort_violations : sig_t -> environment -> formula -> string list =
  fun sgn env fmla -> match fmla with
    | And(fs) -> map_append (sort_violations sgn env) fs
    | Or(fs) -> map_append (sort_violations sgn env) fs
    | Not(f) -> sort_violations sgn env f
    | Implies(lhs,rhs) ->
      let viols = sort_violations sgn env in
      viols lhs @ viols rhs
    | Iff(lhs,rhs) ->
      let viols = sort_violations sgn env in
      viols lhs @ viols rhs
    | Exists(v,s,f) -> sort_violations sgn ((v,s)::env) f
    | Forall(v,s,f) -> sort_violations sgn ((v,s)::env) f
    | Equals(lhs,rhs) ->
      (* todo: check for connected components *)
      let viols = term_sort_violations sgn env in
      let lhs_msgs, lhs_sort = viols lhs in
      let rhs_msgs, rhs_sort = viols rhs in
      lhs_msgs @ rhs_msgs
    | Atom(name, args) ->
      let res = List.map (term_sort_violations sgn env) args in
      let msgs = map_append fst res in
      let used_arity = List.map snd res in
      let expected_arity = Signature.pred_arity sgn name in
      if is_subsort sgn expected_arity used_arity then msgs else
      ("Predicate " ^ name ^ " expects arity (" ^ comma_delim expected_arity ^
       ") but was used with arity (" ^ comma_delim used_arity ^ ").") :: msgs

let well_sorted sgn env fmla = List.is_empty (sort_violations sgn env fmla)

let check_sort_violations sgn env fmla =
  let msgs = sort_violations sgn env fmla in
  match msgs with
  | m::_ -> raise (Signature_violation m)
  | [] -> ()

let well_formed  : sig_t -> environment -> formula -> bool =
  fun sgn env fmla ->
    meets_signature sgn (List.map fst env) fmla && well_sorted sgn env fmla
