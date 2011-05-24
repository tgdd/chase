open Graph
open VocabSyntax
open ListUtil
open Fol

module StringGraph = Persistent.Digraph.Concrete(GraphUtil.StringVertex)
module StringGraphOper = Oper.P(StringGraph)
module StringGraphUtil = GraphUtil.Make(StringGraph)
open StringGraphUtil
module VertexSet = StringGraphUtil.VertexSet
module StringSet = Set.Make(String)


(** Reduces a compound type_decl to multiple simple type_decls, so that
    each one is either just the type name, or the typename and one subtype.
    After applying this to the whole list of type_decls, many types will
    be represented more than once. *)
let reduce_type : type_decl -> type_decl list =
  fun t ->
    { type_name = t.type_name; subtypes = [] } ::
    (map_append (fun st -> [ { type_name = t.type_name
                             ; subtypes = [st]
                             }
                           ; { type_name = st
                             ; subtypes = []
                             } ])
                t.subtypes)

(** Creates a graph representation of the subtype relation *)
let type_graph : type_decl list -> StringGraph.t =
  fun ts ->
    let ts = map_append reduce_type ts in
    let add_type_to_graph =
      (fun g t -> match t.subtypes with 
        | [] -> StringGraph.add_vertex g t.type_name
        | sub::sts -> StringGraph.add_edge g t.type_name sub) in
    let g = List.fold_left add_type_to_graph StringGraph.empty ts in
    StringGraphOper.transitive_closure g


(** Creates a formula that says that t is a supertype of n *)
let supertype t n =
  Forall("x", Implies(Pred(n, [Var("x")]),
                              Pred(t, [Var("x")])))

(** Creates a formula that says t is disjoint from n *) 
let disjoint t n =
  Forall("x", Implies(And([Pred(n, [Var("x")])
                          ;Pred(t, [Var("x")])]),
                      False))    

(** Creates the type constraints for the type declarations.
    Does so by first turning the type delcarations into simple type
    declarations so that each sort A appears as "(Type A)" and each
    subsort relation A > B appears as "(Type A > B)", and nothing else.
    Then turns the type relations into a directed graph and finds the
    transitive closure of that graph.
    
    From the graph, we determine if each pair of sorts requires disjointness or
    subsort constraints to be imposed (in FOL). Since we have already
    taken the transitive closure of the graph, subsort constraints are
    necessary precisely when A->B or B->A appear in the graph. Disjointness
    constraints are necessary when none of A->B, B->C, (A->C and B->C) appear
    in the graph. In the case that A is disjoint from B, all of B's subsorts
    can be omitted from the search for more disjointness constraints, since
    they will be covered by the disjointness constraint between A and B and
    the subsort constraint between B and its subsorts.
    
    Disjointness relations appear as "forall x . A(x) and B(x) -> FALSE".
    Subsort relations appear as "forall x . B(x) -> A(x)". *)
let compile_types : type_decl list -> theory =
  fun ts ->
    let g = (type_graph ts) in
    let acc = ref [] in
    let vertices = vertex_set g in
    let finished = ref VertexSet.empty in
    let all_nodes = Queue.create() in
    VertexSet.iter (fun e -> Queue.add e all_nodes) vertices;
    while not (Queue.is_empty all_nodes) do
      let t = Queue.pop all_nodes in
      finished := VertexSet.add t !finished;
(*      acc := Exists("x",Pred(t,[Var("x")]))::!acc; (* non-empty sorts *) *)
	    let q = Queue.create() in
	    let fin = ref VertexSet.empty in
      VertexSet.iter (fun e -> Queue.add e q) vertices;
      while not (Queue.is_empty q) do
        let n = Queue.pop q in
        if VertexSet.mem n !fin || VertexSet.mem n !finished 
        then () 
        else begin
          fin := VertexSet.add n !fin;
          (* t is a supertype of n *)
          if (StringGraph.mem_edge g t n)
          then acc := (supertype t n)::!acc
          (* n is a supertype of t *)
          else if (StringGraph.mem_edge g n t)
          then acc := (supertype n t)::!acc 
          (* n and t are incomparable but share children *)
          else if not (share_child g t n)
          (* n and t are incomparable with no shared children, so disjoint *) 
          then ( acc := ( (disjoint t n)::!acc)
               ; fin := (VertexSet.union (successors g n) !fin) )
        end
      done;
    done;
    !acc
