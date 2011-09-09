open Graph

(* graphs *)

(** For graphs with strings as the verticies. *)
module StringVertex =
  struct 
    include String
    let equal a b = a = b
    let hash a = Hashtbl.hash a
  end
  
module Edge = functor (A : Set.OrderedType) ->
  struct 
    type t = A.t * A.t
    let compare lhs rhs =
      let res = (A.compare (fst lhs) (fst rhs)) in
      if res < 0 then -1
      else if res > 0 then 1
      else A.compare (snd lhs) (snd rhs)
  end

(** Utility functions for graphs *)
module Make =
	functor (G: Graph.Sig.G) ->
	  struct
      module VertexSet = Set.Make(G.V)
			module EdgeSet = Set.Make(Edge(G.V))
      
      (** Gets the set of verticies in a graph. *)
			let vertex_set g = G.fold_vertex VertexSet.add g VertexSet.empty
			
      let edge_set g = G.fold_edges (fun a b s -> EdgeSet.add (a,b) s) g EdgeSet.empty
      
			(** Gets the set of successors of a node. *)
			let successors : G.t -> G.V.t -> VertexSet.t =
			  fun g a ->
			    G.fold_succ VertexSet.add g a VertexSet.empty
			
			(** Determines if two nodes share children. *)
			let share_child : G.t -> G.V.t -> G.V.t -> bool =
			  fun g a b -> not (VertexSet.is_empty (VertexSet.inter (successors g a)
		                                                          (successors g b)))
      let equal a b = VertexSet.equal (vertex_set a) (vertex_set b)
                          && EdgeSet.equal (edge_set a) (edge_set b)
    end