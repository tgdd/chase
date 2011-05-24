open VocabSyntax
open Fol
open ListUtil

(** Creates type constraints for the specified function. *)
let type_constraint : func -> formula =
  fun f -> match f with
    (* Handle constants more cleanly. For "a: A" produces "A(a)". *)
    | { func_name = name
      ; func_arg_types = []
      ; func_result_type = restype } ->
      Pred(restype, [FunApp(name,[])])
    (* For f: A->B->C produces A(v1) and B(v2) -> C(f(v1,v2)) *)
    | { func_name = name
      ; func_arg_types = argtypes
      ; func_result_type = restype } ->
      let i = ref 0 in
      let type2var t = incr i; "v" ^ string_of_int !i in
      let var_names = List.map type2var argtypes in
      let terms = List.map (fun v -> Var v) var_names in
      let preds = List.map2 (fun t v -> Pred(t,[v])) argtypes terms in
      let lhs = And preds in
      let rhs = Pred(restype, [FunApp(name, terms)]) in
      let body = Implies(lhs, rhs) in
      List.fold_left (fun fmla v -> Forall(v, fmla)) body var_names

let compile_funcs = List.map type_constraint
