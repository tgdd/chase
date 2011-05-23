(* Time-stamp: "Sun 6/6/10 14:52 Dan Dougherty fol-parse-print.ml"  *)
(* Copyright (c) 2003-2007, John Harrison. (See "LICENSE.txt" for details.)  *)

(* 
Parsing of formulas

Printing of formulas

USES THE FOLLOWING TYPE DEFINITIONS
[Harrison wants to work with prop + fol, so neds to distinguish two kinds of formula.
Too fussy --- simpler here]

type variable = string;;

type term = Var of variable
          | Fn of string * term list;;

type atomicFormula = string * term list;;

type folFormula = False
                 | True
                 | Atom of atomicFormula
                 | Not of folFormula
                 | And of folFormula * folFormula
                 | Or of folFormula * folFormula
                 | Imp of folFormula * folFormula
                 | Iff of folFormula * folFormula
                 | Forall of string * folFormula
                 | Exists of string * folFormula;;
*)

(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* Explosion and implosion of strings.                                       *)
(* ------------------------------------------------------------------------- *)

let explode s =
  let rec exap n l =
     if n < 0 then l else
      exap (n - 1) ((String.sub s n 1)::l) in
  exap (String.length s - 1) [];;

let implode l = List.fold_right (^) l "";;

(* ------------------------------------------------------------------------- *)
(* Lexical analysis.                                                         *)
(* ------------------------------------------------------------------------- *)

let matches s = let chars = explode s in fun c -> List.mem c chars;;

let space = matches " \t\n\r"
and punctuation = matches "()[]{},"
and symbolic = matches "~`!@#$%^&*-+=|\\:;<>.?/"
and numeric = matches "0123456789"
and alphanumeric = matches
  "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";;

let rec lexwhile prop inp =
  match inp with
    c::cs when prop c -> let tok,rest = lexwhile prop cs in c^tok,rest
  | _ -> "",inp;;

let rec lex inp =
  match snd(lexwhile space inp) with
    [] -> []
  | c::cs -> let prop = if alphanumeric(c) then alphanumeric
                        else if symbolic(c) then symbolic
                        else fun c -> false in
             let toktl,rest = lexwhile prop cs in
             (c^toktl)::lex rest;;

(* ------------------------------------------------------------------------- *)
(* Generic function to impose lexing and exhaustion checking on a parser.    *)
(* ------------------------------------------------------------------------- *)

let make_parser pfn s =
  let expr,rest = pfn (lex(explode s)) in
  if rest = [] then expr else failwith "Unparsed input";;



(* ------------------------------------------------------------------------- *)
(* General parsing of iterated infixes.                                      *)
(* ------------------------------------------------------------------------- *)

let rec parse_ginfix opsym opupdate sof subparser inp =
  let e1,inp1 = subparser inp in
  if inp1 <> [] & List.hd inp1 = opsym then
     parse_ginfix opsym opupdate (opupdate sof e1) subparser (List.tl inp1)
  else sof e1,inp1;;

let parse_left_infix opsym opcon =
  parse_ginfix opsym (fun f e1 e2 -> opcon(f e1,e2)) (fun x -> x);;

let parse_right_infix opsym opcon =
  parse_ginfix opsym (fun f e1 e2 -> f(opcon(e1,e2))) (fun x -> x);;

let parse_list opsym =
  parse_ginfix opsym (fun f e1 e2 -> (f e1)@[e2]) (fun x -> [x]);;

(* ------------------------------------------------------------------------- *)
(* Other general parsing combinators.                                        *)
(* ------------------------------------------------------------------------- *)

let papply f (ast,rest) = (f ast,rest);;

let nextin inp tok = inp <> [] & List.hd inp = tok;;

let parse_bracketed subparser cbra inp =
  let ast,rest = subparser inp in
  if nextin rest cbra then ast,List.tl rest
  else failwith "Closing bracket expected";;


(* ------------------------------------------------------------------------- *)
(* Parsing of terms.                                                         *)
(* ------------------------------------------------------------------------- *)

let is_const_name s = List.for_all numeric (explode s) or s = "nil";;

let rec parse_atomic_term vs inp =
  match inp with
    [] -> failwith "term expected"
  | "("::rest -> parse_bracketed (parse_term vs) ")" rest
  | "-"::rest -> papply (fun t -> Fn("-",[t])) (parse_atomic_term vs rest)
  | f::"("::")"::rest -> Fn(f,[]),rest
  | f::"("::rest ->
      papply (fun args -> Fn(f,args))
             (parse_bracketed (parse_list "," (parse_term vs)) ")" rest)
  | a::rest ->
      (if is_const_name a & not(List.mem a vs) then Fn(a,[]) else Var a),rest

and parse_term vs inp =
  parse_right_infix "::" (fun (e1,e2) -> Fn("::",[e1;e2]))
    (parse_right_infix "+" (fun (e1,e2) -> Fn("+",[e1;e2]))
       (parse_left_infix "-" (fun (e1,e2) -> Fn("-",[e1;e2]))
          (parse_right_infix "*" (fun (e1,e2) -> Fn("*",[e1;e2]))
             (parse_left_infix "/" (fun (e1,e2) -> Fn("/",[e1;e2]))
                (parse_left_infix "^" (fun (e1,e2) -> Fn("^",[e1;e2]))
                   (parse_atomic_term vs)))))) inp;;

let parset = make_parser (parse_term []);;

(* ------------------------------------------------------------------------- *)
(* Parsing of folFormulas, parametrized by atom parser "pfn".                   *)
(* ------------------------------------------------------------------------- *)

let rec parse_atomic_formula (ifn,afn) vs inp =
  match inp with
    [] -> failwith "formula expected"
  | "false"::rest -> False,rest
  | "true"::rest -> True,rest
  | "("::rest -> (try ifn vs inp with Failure _ ->
                  parse_bracketed (parse_formula (ifn,afn) vs) ")" rest)
  | "~"::rest -> papply (fun p -> Not p)
                        (parse_atomic_formula (ifn,afn) vs rest)
  | "forall"::x::rest ->
        parse_quant (ifn,afn) (x::vs) (fun (x,p) -> Forall(x,p)) x rest
  | "exists"::x::rest ->
        parse_quant (ifn,afn) (x::vs) (fun (x,p) -> Exists(x,p)) x rest
  | _ -> afn vs inp

and parse_quant (ifn,afn) vs qcon x inp =
   match inp with
     [] -> failwith "Body of quantified term expected"
   | y::rest ->
        papply (fun fm -> qcon(x,fm))
               (if y = "." then parse_formula (ifn,afn) vs rest
                else parse_quant (ifn,afn) (y::vs) qcon y rest)

and parse_formula (ifn,afn) vs inp =
   parse_right_infix "<=>" (fun (p,q) -> Iff(p,q))
     (parse_right_infix "==>" (fun (p,q) -> Imp(p,q))
         (parse_right_infix "\\/" (fun (p,q) -> Or(p,q))
             (parse_right_infix "/\\" (fun (p,q) -> And(p,q))
                  (parse_atomic_formula (ifn,afn) vs)))) inp;;


(* ------------------------------------------------------------------------- *)
(* Parsing of formulas.                                                      *)
(* ------------------------------------------------------------------------- *)

let parse_infix_atom vs inp =       
  let tm,rest = parse_term vs inp in
  if List.exists (nextin rest) ["="; "<"; "<="; ">"; ">="] then                     
        papply (fun tm' -> Atom(List.hd rest,[tm;tm']))
               (parse_term vs (List.tl rest))                                       
  else failwith "";;
                                                               
let parse_atom vs inp =
  try parse_infix_atom vs inp with Failure _ ->                                
  match inp with                                                               
  | p::"("::")"::rest -> Atom(p,[]),rest                                    
  | p::"("::rest ->
      papply (fun args -> Atom(p,args))
             (parse_bracketed (parse_list "," (parse_term vs)) ")" rest)
  | p::rest when p <> "(" -> Atom(p,[]),rest
  | _ -> failwith "parse_atom";;
                                                                               
(* ------------------------------------------------------------------------- *)
(* Set up parsing of quotations.                                             *)
(* ------------------------------------------------------------------------- *)

let parse = make_parser                                                        
  (parse_formula (parse_infix_atom,parse_atom) []);;              

(* Is it here that we make the connection with << .. >> quotation?? *)
let default_parser = parse;;
let secondary_parser = parset;;


(* ------------------------------------------------------------------------- *)
(* Printing of terms.                                                        *)
(* ------------------------------------------------------------------------- *)

let rec print_term prec fm =
  match fm with
    Var x -> print_string x
  | Fn("^",[tm1;tm2]) -> print_infix_term true prec 24 "^" tm1 tm2
  | Fn("/",[tm1;tm2]) -> print_infix_term true prec 22 " /" tm1 tm2
  | Fn("*",[tm1;tm2]) -> print_infix_term false prec 20 " *" tm1 tm2
  | Fn("-",[tm1;tm2]) -> print_infix_term true prec 18 " -" tm1 tm2
  | Fn("+",[tm1;tm2]) -> print_infix_term false prec 16 " +" tm1 tm2
  | Fn("::",[tm1;tm2]) -> print_infix_term false prec 14 "::" tm1 tm2
  | Fn(f,args) -> print_fargs f args

and print_fargs f args =
  print_string f;
  if args = [] then () else
   (print_string "(";
    open_box 0;
    print_term 0 (List.hd args); print_break 0 0;
    List.iter (fun t -> print_string ","; print_break 0 0; print_term 0 t)
            (List.tl args);
    close_box();
    print_string ")")

and print_infix_term isleft oldprec newprec sym p q =
  if oldprec > newprec then (print_string "("; open_box 0) else ();
  print_term (if isleft then newprec else newprec+1) p;
  print_string sym;
  print_break (if String.sub sym 0 1 = " " then 1 else 0) 0;
  print_term (if isleft then newprec+1 else newprec) q;
  if oldprec > newprec then (close_box(); print_string ")") else ();;

let printert tm =
  open_box 0; print_string "<<|";
  open_box 0; print_term 0 tm; close_box();
  print_string "|>>"; close_box();;

#install_printer printert;;


(* ------------------------------------------------------------------------- *)
(* Printing of formulas, parametrized by atom printer.                       *)
(* ------------------------------------------------------------------------- *)

let bracket p n f x y =
  (if p then print_string "(" else ());
  open_box n; f x y; close_box();
  (if p then print_string ")" else ());;

let rec strip_quant fm =
  match fm with
    Forall(x,(Forall(y,p) as yp)) | Exists(x,(Exists(y,p) as yp)) ->
        let xs,q = strip_quant yp in x::xs,q
  |  Forall(x,p) | Exists(x,p) -> [x],p
  | _ -> [],fm;;

let print_formula pfn =
  let rec print_formula pr fm =
    match fm with
      False -> print_string "false"
    | True -> print_string "true"
    | Atom(pargs) -> pfn pr pargs
    | Not(p) -> bracket (pr > 10) 1 (print_prefix 10) "~" p
    | And(p,q) -> bracket (pr > 8) 0 (print_infix 8 "/\\") p q
    | Or(p,q) ->  bracket (pr > 6) 0 (print_infix  6 "\\/") p q
    | Imp(p,q) ->  bracket (pr > 4) 0 (print_infix 4 "==>") p q
    | Iff(p,q) ->  bracket (pr > 2) 0 (print_infix 2 "<=>") p q
    | Forall(x,p) -> bracket (pr > 0) 2 print_qnt "forall" (strip_quant fm)
    | Exists(x,p) -> bracket (pr > 0) 2 print_qnt "exists" (strip_quant fm)
  and print_qnt qname (bvs,bod) =
    print_string qname;
    List.iter (fun v -> print_string " "; print_string v) bvs;
    print_string "."; print_space(); open_box 0;
    print_formula 0 bod;
    close_box()
  and print_prefix newpr sym p =
   print_string sym; print_formula (newpr+1) p
  and print_infix newpr sym p q =
    print_formula (newpr+1) p;
    print_string(" "^sym); print_space();
    print_formula newpr q in
  print_formula 0;;

let print_qformula pfn fm =
  open_box 0; print_string "<<";
  open_box 0; print_formula pfn fm; close_box();
  print_string ">>"; close_box();;


(* ------------------------------------------------------------------------- *)
(* Printing of formulas.                                                     *)
(* ------------------------------------------------------------------------- *)

let print_atom prec (p,args) =
  if List.mem p ["="; "<"; "<="; ">"; ">="] & List.length args = 2
(*  then print_infix_term false 12 12 (" "^p) (el 0 args) (el 1 args) *)
  then print_infix_term false 12 12 (" "^p) (List.hd args) (List.hd (List.tl args))
  else print_fargs p args;;

let print_fol_formula = print_qformula print_atom;;

#install_printer print_fol_formula;;
