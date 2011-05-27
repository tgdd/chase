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
type signature =
  (** The names of the sorts. *)
  { sort_names : sort_t list
  (** Subsort relation, lhs supersort of rhs *)
  ; subsorts : (sort_t * sort_t) list
  (** The function names. *)
  ; func_names : func_t list
  (** The ranks of the function. *)
  ; func_rank : func_t -> sort_t list * sort_t
  (** The names of the predicates. *)
  ; pred_names : pred_t list
  (** The airty of the predicates. *)
  ; pred_airty : pred_t -> sort_t list
  }

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
val signature_violations : signature -> var_t list -> formula -> string list

(** Gets a list of messages describing the sort violations. The second argument
    is a list of the names and types of the free variables. This function
    assumes that signature_violations has been called on it and returned no
    messages. The behavior isundefined otherwise. *)
val sort_violations : signature -> (var_t * sort_t) list -> formula -> string list

(** Determines if a formula is well-formed. The formula is well formed if
    - all predicates, functions, sorts and free variables appear in the
      signature, and
	  - the formula is well-sorted *)
val well_formed : signature -> (var_t * sort_t) list -> formula -> bool

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
