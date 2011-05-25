include VocabSyntax

let read_vocab ic =
  let lexbuf = Lexing.from_channel ic in
  VocabParser.start VocabLexer.token lexbuf
