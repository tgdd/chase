(** Packaged up types and function definitions for vocabularies. *)

module Syntax :
  sig
    (** Vocab type declaration. *)
    type sort = { sort_name : string
                ; subsorts : string list
                }

    (** Vocab predicate declaration. *)
    type pred = { pred_name : string
                ; pred_arity : string list
                }

    (** Vocab constant declaration. *)
    type const = { const_name : string
                 ; const_sort : string
                 }

    (** Vocab function declaration. *)
    type func = { func_name : string
                ; func_arity : string list
                ; func_sort : string
                }

    (** Vocab definition. *)
    type vocab = { vocab_name : string
                 ; sorts : sort list
                 ; preds : pred list
                 ; consts : const list
                 ; funcs : func list
                 }
  end
  
(** Reads a vocab from the given channel. *)
val read_vocab : in_channel -> Syntax.vocab

(** Creates a string representation of a vocab *)
val show_vocab : Syntax.vocab -> string

(** Writes the string representation of the vocab to the specified channel. *)
val output_vocab : out_channel -> Syntax.vocab -> unit

(** Prints the string representation of the vocab to standard out. *)
val print_vocab : Syntax.vocab -> unit

module Compiling :
  sig
    (** Turns a vocab into a sorted FOL signature. *)
    val vocab2signature : Syntax.vocab -> SortedFol.sig_t
  end
