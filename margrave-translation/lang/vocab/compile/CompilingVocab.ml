open ListUtil
open Fol
open VocabSyntax

open CompilingTypes
open CompilingFuncs

(** Turns constants into nullary functions *)
let const2func : const -> func =
  fun c ->
    { func_name = c.const_name
    ; func_arg_types = []
    ; func_result_type = c.const_type
    }

(** Turns a vocabulary into a set of constraints on functions and the
    predicates that represent the types in single-sorted FOL. *)
let compile_vocab : vocab -> theory =
  fun v ->
      compile_types v.types @
      compile_funcs (v.funcs @ List.map const2func v.consts) 
      
