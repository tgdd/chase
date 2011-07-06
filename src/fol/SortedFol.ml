(** Module for sorted first order logic type definitions and utilities.

    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

module Syntax = SortedFolSyntax
module Parser = SortedFolParser
module Lexer = SortedFolLexer

(******************************************************************************)
(* For parsing formulas *)
(** Parses a string to produce a theory *)
let string2theory : string -> Syntax.theory = 
  fun s ->
    let lexbuf = Lexing.from_string s in
    Parser.parse_theory Lexer.token lexbuf

(** Parses a string to produce a formula. Does not check to see if the string
    contains multiple formulas or no formulas, just parses and returns the
    first one in the list. *)
let string2formula : string -> Syntax.formula =
  fun s ->
    let lexbuf = Lexing.from_string s in
    Parser.parse_formula Lexer.token lexbuf

(** Parses an input channel to produce a theory *)
let in_channel2theory ic =
  let lexbuf = Lexing.from_channel ic in
  Parser.parse_theory Lexer.token lexbuf
(* END For parsing formulas *)
(******************************************************************************)
