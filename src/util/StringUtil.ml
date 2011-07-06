(* String processing *)

(** Turns list of strings into a string of lines. *)
let unlines = String.concat "\n"

(** Turns a list of strings into a string of words. *)
let unwords = String.concat " "

(** Turns a list of strings into a string of comma delimited words. *)
let comma_delim = String.concat ", "

(** Braces the body with the left and right strings. *)
let brace_with left right body = left ^ body ^ right

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


(* StringMaps *)
module StringMap = Map.Make(String)

(* StringSets *)
module StringSet = Set.Make(String)
let list2string_set xs = List.fold_left (fun s e -> StringSet.add e s) StringSet.empty xs