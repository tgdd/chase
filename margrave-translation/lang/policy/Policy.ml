include PolicySyntax

let read_policy ic =
  let lexbuf = Lexing.from_channel ic in
  PolicyParser.start PolicyLexer.token lexbuf

let output_policy oc p =
  output_string oc (show_policy p);
  output_string oc "\n";
  flush oc

let print_policy p =
  output_policy stdout p
