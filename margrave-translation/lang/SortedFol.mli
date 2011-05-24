(** Module for sorted first order logic type definitions and utilities.
    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

(** The type for sort names *)
type sort_t = string

(** The type for function and predicate names *)
type name_t = string

(** The type for variable names *)
type var_t = string

(** Terms in sorted FOL. Constants are represented as functions of no
    arguments. *)
type term = Var of var_t 
          | FunApp of name_t * term list

(** Formulas in sorted FOL. *)
type formula = And of formula list
             | Or of formula list
             | Not of formula
             | Implies of formula * formula
             | Iff of formula * formula
             | Exists of name_t * sort_t * formula
             | Forall of name_t * sort_t * formula
             | Equals of term * term
             | Pred of var_t * term list
  
(** Shorthand for True which is represented as the empty conjunction. *)
val tru : formula

(** Shorthand for False which is represented as the empty disjunction. *)
val fals : formula

type theory = formula list

(** Flattens nested Ands and Ors. *)
val flatten_and_or : formula -> formula


(* For displaying formulas and terms *)

(** Displays a plaintext representation of a term. *)
val show_term : term -> string

(** Displays a plaintext representation of a formula *)
val show_formula : formula -> string

(** Prints a list of formulas in plaintext to the channel, one per line *)
val output_formulas : out_channel -> formula list -> unit

(** Displays a LateX representation of the term *)
val latex_term : term -> string

(** Displays a LaTeX representation of the formula. *)
val latex_formula : formula -> string

(** Prints a list of formulas in LaTeX to the channel, one per line *)
val output_latex_formulas : out_channel -> formula list -> unit
