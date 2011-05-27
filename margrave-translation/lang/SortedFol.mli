(** Module for sorted first order logic type definitions and utilities.
    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

(** The type for sort names *)
type sort_t = string

(** The type for function names *)
type func_t = string

(** The type for predicate names *)
type pred_t = string

(** The type for variable names *)
type var_t = string

(** Signature in sorted FOL. *)
module Signature :
  sig
    (** Signature type *)
    type t
    (** Empty signature *)
    val empty : t
    (** Add the sort with subsorts to the signature. If the sort/subsort pairs
        are already in the sig, there is no change. *)
    val add_sort : t -> sort_t -> sort_t list -> t
    (** Add the function with the arity and result sort to the signature.
        If the function is already in the sig then Invalid_argument is
        raised. *)
    val add_func : t -> func_t -> sort_t list -> sort_t -> t
    (** Add the predicate witht he arity to the signature. If the pred is
        already in the sig, then Invalid_argument is raised. *)
    val add_pred : t -> pred_t -> sort_t list -> t
    
    (** Whether the signature has the sort name. *)
    val sort_mem : t -> sort_t -> bool
    (** Subsort relation, lhs supersort of rhs *)
    val is_subsort : t -> sort_t -> sort_t -> bool
    (** The function names. *)
    val func_mem : t -> func_t -> bool
    (** The ranks of the function. *)
    val func_rank : t -> func_t -> sort_t list * sort_t
    (** The names of the predicates. *)
    val pred_mem : t -> pred_t -> bool
    (** The arity of the predicates. *)
    val pred_arity : t -> pred_t -> sort_t list
    (** Validates the signature, ensuring that all sorts are represented.
        The list returns are the messages describing the errors. *)
    val validate : t -> string list
    (** Validates the signature, ensuring that all sorts are represented. *)
    val is_valid : t -> bool
  end
  
(** Shorthand for signature type *)
type sig_t = Signature.t

(** Terms in sorted FOL. Constants are represented as functions of no
    arguments. *)
type term = Var of var_t 
          | FunApp of func_t * term list

(** Formulas in sorted FOL. *)
type formula = And of formula list
             | Or of formula list
             | Not of formula
             | Implies of formula * formula
             | Iff of formula * formula
             | Exists of var_t * sort_t * formula
             | Forall of var_t * sort_t * formula
             | Equals of term * term
             | Pred of pred_t * term list
  
(** Shorthand for True which is represented as the empty conjunction. *)
val tru : formula

(** Shorthand for False which is represented as the empty disjunction. *)
val fals : formula

type theory = formula list

(** Determines the free variables present in a formula. *)
val free_vars : formula -> var_t list

(** Determines if a formula is a sentence (has no free variables). *)
val is_sentence : formula -> bool

(** Gets a list of messages describing the signature violations. The first
    argument is a list of the variables that are allowed to be free. *)
val signature_violations : Signature.t -> var_t list -> formula -> string list

(** Gets a list of messages describing the sort violations. The second argument
    is a list of the names and types of the free variables. This function
    assumes that signature_violations has been called on it and returned no
    messages. The behavior isundefined otherwise. *)
val sort_violations : sig_t -> (var_t * sort_t) list -> formula -> string list

(** Determines if a formula is well-formed. The formula is well formed if
    - all predicates, functions, sorts and free variables appear in the
      signature, and
	  - the formula is well-sorted *)
val well_formed : sig_t -> (var_t * sort_t) list -> formula -> bool

(** Attempts to simplify the formula (while maintaining logical equivalence). *)
val simplify : formula -> formula

(* For displaying formulas and terms *)

(** Displays a plaintext representation of a formula *)
val show_formula : formula -> string

(** Prints a list of formulas in plaintext to the channel, one per line *)
val output_formulas : out_channel -> formula list -> unit

(** Displays a LaTeX representation of the formula. *)
val show_latex_formula : formula -> string

(** Prints a list of formulas in LaTeX to the channel, one per line *)
val output_latex_formulas : out_channel -> formula list -> unit
