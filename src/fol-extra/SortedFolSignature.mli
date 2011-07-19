(** Module for sorted first order logic type definitions and utilities.

    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

open SortedFolFormulas
exception Signature_violation of string


(* Signature in sorted FOL. *)
module Signature : sig
    (** Signature type *)
    type sig_t
    (** Initial signature from list of sorts and subsort relation. *)
    val init : sort_t list -> (sort_t * sort_t) list -> sig_t
    (** Add the function with the arity and result sort to the signature.
        If the function is already in the sig then Invalid_argument is
        raised. *)
    val add_func : sig_t -> func_t -> sort_t list -> sort_t -> sig_t
    (** Add the predicate witht he arity to the signature. If the pred is
        already in the sig, then Invalid_argument is raised. *)
    val add_pred : sig_t -> pred_t -> sort_t list -> sig_t
    
    (** Whether the signature has the sort name. *)
    val mem_sort : sig_t -> sort_t -> bool
    (** Subsort relation, lhs supersort of rhs *)
    val is_subsort : sig_t -> sort_t -> sort_t -> bool
    (** The function names. *)
    val mem_func : sig_t -> func_t -> bool
    (** The ranks of the function. *)
    val func_rank : sig_t -> func_t -> sort_t list * sort_t
    (** The names of the predicates. *)
    val mem_pred : sig_t -> pred_t -> bool
    (** The arity of the predicates. *)
    val pred_arity : sig_t -> pred_t -> sort_t list
end
type sig_t = Signature.sig_t

(** Determines the free variables present in a formula. *)
val free_vars : formula -> var_t list

(** Determines if a formula is a sentence (has no free variables). *)
val is_sentence : formula -> bool

(** Gets a list of messages describing the signature violations. The first
    argument is a list of the variables that are allowed to be free. *)
val signature_violations : sig_t -> var_t list -> formula -> string list

(** Throws Signature_violation if there is one. See signature_violations. *)
val check_signature_violations : sig_t -> var_t list -> formula -> unit

(** Gets a list of messages describing the sort violations. The second argument
    is a list of the names and types of the free variables. This function
    assumes that signature_violations has been called on it and returned no
    messages. The behavior isundefined otherwise. *)
val sort_violations : sig_t -> (var_t * sort_t) list -> formula -> string list

(** Throws Signature_violation if there is one. See sort_violations. *)
val check_sort_violations : sig_t -> (var_t * sort_t) list -> formula -> unit

(** Determines if a formula is well-formed. The formula is well formed if
    - all predicates, functions, sorts and free variables appear in the
      signature, and
	  - the formula is well-sorted *)
val well_formed : sig_t -> (var_t * sort_t) list -> formula -> bool
