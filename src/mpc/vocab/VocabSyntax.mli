(** Defines types for the AST of a vocab, as well as functions for reading a
    vocab definition from a channel, converting a policy AST to a string, and
    printing or outputting a policy.
    
    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

(******************************************************************************)
(* AST TYPES *)
(** Sort and subsort declaration. *)
(* (Type A > B C) (Type D) mean sort A is a supersort of B and C, D is a sort *)
type sort = { sort_name : string
            ; subsorts : string list
            }

(** Predicate declaration. *)
(* (Pred R A B C) means predicate R is of sort AxBxC *)
type pred = { pred_name : string
            ; pred_arity : string list
            }

(** Constant declaration. *)
(* (Const x A) means x is a constant of type A *)
type const = { const_name : string
             ; const_sort : string
             }

(** Function declaration. *)
(* (Function f A B C) means f is a function from AxB to C *)
type func = { func_name : string
            ; func_arity : string list
            ; func_sort : string
            }

(** Vocab definition. *)
(* a vocab is a combination of the above "(" "Vocab" <id> <TYPES-CLAUSE>   
   <PREDS-CLAUSE> <CONSTANTS-CLAUSE> <FUNCTIONS-CLAUSE> ")"                *)
type vocab = { vocab_name : string
             ; sorts : sort list
             ; preds : pred list
             ; consts : const list
             ; funcs : func list
             }
(* END AST TYPES *)
(******************************************************************************)

(******************************************************************************)
(* SHOW *)
(** Produces a prettified string representation of the vocab *)
val show_vocab :  vocab -> string
(* END SHOW *)
(******************************************************************************)
