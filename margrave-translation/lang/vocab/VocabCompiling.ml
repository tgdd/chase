open Graph
module VS = VocabSyntax
module SF = SortedFol

module StringGraph = Persistent.Digraph.Concrete(GraphUtil.StringVertex)
module SortGraph = StringGraph
module StringGraphOper = Oper.P(StringGraph)
module SortGraphOper = StringGraphOper
module StringSet = Set.Make(String)

let sort2names s =
  s.VS.sort_name :: s.VS.subsorts

let sort2subsorts s =
  List.map (fun sub -> (s.VS.sort_name, sub)) s.VS.subsorts

let add_pred sgn p =
  SF.Signature.add_pred sgn p.VS.pred_name p.VS.pred_arity

let add_const sgn c =
  SF.Signature.add_func sgn c.VS.const_name [] c.VS.const_sort

let add_func sgn f =
  SF.Signature.add_func sgn f.VS.func_name f.VS.func_arity f.VS.func_sort

let vocab2signature : VS.vocab -> SF.sig_t =
  fun { VS.vocab_name = name
      ; VS.sorts = sorts
      ; VS.preds = preds
      ; VS.consts = consts
      ; VS.funcs = funcs
      } ->
    let sort_names = ListUtil.map_append sort2names sorts in
    let subsorts = ListUtil.map_append sort2subsorts sorts in
    let sgn = SF.Signature.init sort_names subsorts in
    let sgn = List.fold_left add_pred sgn preds in
    let sgn = List.fold_left add_const sgn consts in
    let sgn = List.fold_left add_func sgn funcs in
    sgn