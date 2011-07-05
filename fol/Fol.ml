(** Module for sorted first order logic type definitions and utilities.

    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

module Syntax = FolSyntax

(******************************************************************************)
(* For parsing formulas *)
(** Parses a string to produce a theory *)
let string2theory : string -> Syntax.theory = 
  fun s ->
    let lexbuf = Lexing.from_string s in
    FolParser.parse_theory FolLexer.token lexbuf

(** Parses a string to produce a formula. Does not check to see if the string
    contains multiple formulas or no formulas, just parses and returns the
    first one in the list. *)
let string2formula s =
  List.hd (string2theory s)

(** Parses an input channel to produce a theory *)
let in_channel2theory ic =
  let lexbuf = Lexing.from_channel ic in
  FolParser.parse_theory FolLexer.token lexbuf
(* END For parsing formulas *)
(******************************************************************************)
