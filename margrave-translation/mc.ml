open IOUtil
open StringUtil
open CompilingVocab
open CompilingPolicy

(** Write the formulas on the given output channel. *)
let output_formulas formulas oc =
  	output_string oc (unlines (List.map Fol.show_formula formulas))

(** Reads a vocab definition from the specified input channel. *)
let read_vocab ic =
  let lexbuf = Lexing.from_channel ic in
	VocabParser.start VocabLexer.token lexbuf

(** Reads a policy definition from the specified input channel. *)
let read_policy ic =
  let lexbuf = Lexing.from_channel ic in
  PolicyParser.start PolicyLexer.token lexbuf
  
let main () =
  begin
	  if (Array.length Sys.argv != 3) then raise (Arg.Bad "bad args");

    let infile = Array.get Sys.argv 1 in
    let outfile = Array.get Sys.argv 2 in

    if ends_with infile ".vocab" then
      let voc = call_with_in_channel infile read_vocab in
      call_with_out_channel outfile (output_formulas (compile_vocab voc))
    else if ends_with infile ".policy" then
      let pol = call_with_in_channel infile read_policy in
      call_with_out_channel outfile (output_formulas (compile_policy pol))
    else
      raise (Arg.Bad "bad filetype")
  end

let _ = Printexc.print main ()