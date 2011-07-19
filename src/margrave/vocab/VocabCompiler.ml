(** Type and functions for compiling a vocab into useful metadata.

    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)
open Graph
module VS = VocabSyntax
module SF = SortedFol.Syntax
module Signature = SortedFol.Signature

module StringGraph = Persistent.Digraph.Concrete(GraphUtil.StringVertex)
module SortGraph = StringGraph
module StringGraphOper = Oper.P(StringGraph)
module SortGraphOper = StringGraphOper
module StringSet = Set.Make(String)

(** Converts a sort declaration to a  list of sort names. *)
let sort2names s =
  s.VS.sort_name :: s.VS.subsorts

(** Converts a sort declaration into a list of sort, subsort pairs. *)
let sort2subsorts s =
  List.map (fun sub -> (s.VS.sort_name, sub)) s.VS.subsorts

(** Adds the specified predicate to the signature. *)
let add_pred sgn p =
  Signature.Signature.add_pred sgn p.VS.pred_name p.VS.pred_arity

(** Adds the specified function to the signature as a nullary function. *)
let add_const sgn c =
  Signature.Signature.add_func sgn c.VS.const_name [] c.VS.const_sort

(** Adds the specified function to the signature. *)
let add_func sgn f =
  Signature.Signature.add_func sgn f.VS.func_name f.VS.func_arity f.VS.func_sort

(** Converts a vocab to a Sorted FOL siganture. *)
let vocab2signature : VS.vocab -> Signature.sig_t =
  fun { VS.vocab_name = name
      ; VS.sorts = sorts
      ; VS.preds = preds
      ; VS.consts = consts
      ; VS.funcs = funcs
      } ->
    let sort_names = ListUtil.map_append sort2names sorts in
    let subsorts = ListUtil.map_append sort2subsorts sorts in
    let sgn = Signature.Signature.init sort_names subsorts in
    let sgn = List.fold_left add_pred sgn preds in
    let sgn = List.fold_left add_const sgn consts in
    let sgn = List.fold_left add_func sgn funcs in
    sgn
