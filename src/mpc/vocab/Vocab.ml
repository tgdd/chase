(** Packaged up types and function definitions for vocabularies.

    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

module Syntax = VocabSyntax
module Compiler = VocabCompiler

(******************************************************************************)
(** READING AND WRITING VOCAB HELPERS *)
(** Creates a string representation of a vocab. *)
let show_vocab = Syntax.show_vocab

(** Reads a vocab from the given input channel. *)
let read_vocab ic =
  let lexbuf = Lexing.from_channel ic in
  VocabParser.parse VocabLexer.token lexbuf

(** Reads a vocab from the given string. *)
let string2vocab str =
  let lexbuf = Lexing.from_string str in
  VocabParser.parse VocabLexer.token lexbuf

(** Outputs a vocab to the given output channel. *)
let output_vocab oc v =
  output_string oc (show_vocab v);
  output_string oc "\n";
  flush oc

(** Prints a vocab to standard out. *)
let print_vocab v =
  output_vocab stdout v
(** END READING AND WRITING VOCAB HELPERS *)
(******************************************************************************)
