module Syntax = VocabSyntax
module Compiling = VocabCompiling

let show_vocab = Syntax.show_vocab

let read_vocab ic =
  let lexbuf = Lexing.from_channel ic in
  VocabParser.start VocabLexer.token lexbuf

let output_vocab oc v =
  output_string oc (show_vocab v);
  output_string oc "\n";
  flush oc

let print_vocab v =
  output_vocab stdout v