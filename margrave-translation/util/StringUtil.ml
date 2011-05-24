(* String processing *)

(** Turns list of strings into a string of lines. *)
let unlines = String.concat "\n"

(** Turns a list of strings into a string of words. *)
let unwords = String.concat " "

(** Creates an indentation of the number of levels indicated using spaces. *)
let indent : int -> string =
  fun i ->
    String.make (2*i) ' '

(** Determines if a string ends with a given postfix. *)
let ends_with : string -> string -> bool =
  fun str post ->
      let strlen = String.length str in
      let postlen = String.length post in
      if strlen < postlen then
        false
      else
        let strpost = String.sub str (strlen - postlen) postlen in
        strpost = post
