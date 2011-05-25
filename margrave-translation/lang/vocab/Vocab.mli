(** Packaged up types and function definitions for vocabularies. *)

(** Vocab type declaration. *)
type type_decl = { type_name : string
                 ; subtypes : string list
                 }

(** Vocab predicate declaration. *)
type pred = { pred_name : string
            ; pred_type : string list
            }

(** Vocab constant declaration. *)
type const = { const_name : string
             ; const_type : string
             }

(** Vocab function declaration. *)
type func = { func_name : string
            ; func_arg_types : string list
            ; func_result_type : string
            }

(** Vocab definition. *)
type vocab = { vocab_name : string
             ; types : type_decl list
             ; preds : pred list
             ; consts : const list
             ; funcs : func list
             }

(** Reads a vocab from the given channel. *)
val read_vocab : in_channel -> vocab

(** Creates a string representation of a vocab *)
val show_vocab : vocab -> string

(** Writes the string representation of the vocab to the specified channel. *)
val output_vocab : out_channel -> vocab -> unit

(** Prints the string representation of the vocab to standard out. *)
val print_vocab : vocab -> unit
