open Graph

(* graphs *)

(** For graphs with strings as the verticies. *)
module StringVertex =
  struct 
    include String
    let equal a b = a = b
    let hash a = Hashtbl.hash a
  end

(** Utility functions for graphs *)
module Make =
	functor (G: Graph.Sig.G) ->
	  struct
      module VertexSet = Set.Make(G.V)
			
      (** Gets the set of verticies in a graph. *)
			let vertex_set g = G.fold_vertex VertexSet.add g VertexSet.empty
			
			(** Gets the set of successors of a node. *)
			let successors : G.t -> G.V.t -> VertexSet.t =
			  fun g a ->
			    G.fold_succ VertexSet.add g a VertexSet.empty
			
			(** Determines if two nodes share children. *)
			let share_child : G.t -> G.V.t -> G.V.t -> bool =
			  fun g a b -> not (VertexSet.is_empty (VertexSet.inter (successors g a)
		                                                          (successors g b)))
    end
