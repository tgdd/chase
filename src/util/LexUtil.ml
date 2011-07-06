(** Increments the line number and sets the beggining of line value for the
    lexbuf position *)
let incr_lineno : Lexing.lexbuf -> unit =
  fun lexbuf ->
	    let pos = lexbuf.Lexing.lex_curr_p in
	    lexbuf.Lexing.lex_curr_p <- { pos with
	      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
	      Lexing.pos_bol = pos.Lexing.pos_cnum;
	    }
