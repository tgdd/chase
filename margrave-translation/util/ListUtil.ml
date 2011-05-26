(* List utilities *)

(** Determines if a list is empty. More readable than "= []" *)
let is_empty xs = xs = []

(** Gets all but the last element of a list. *)
let rec init xs = match xs with
  | [x] -> []
  | x::xs -> x :: (init xs)
  | [] -> raise (Failure "init called with empty list")

(** Gets just the last element of a list. *)
let rec last xs = match xs with
  | [x] -> x
  | x::xs -> last xs
  | [] -> raise (Failure "last called with empty list")

(** Map the list of with the given function, and then concatenate the
    results. Equivalent to List.concat (List.map f xs) *)
let map_append : ('a -> 'b list) -> 'a list -> 'b list =
  fun f xs ->
    List.rev (List.fold_left (fun bs a -> List.rev_append (f a) bs ) [] xs)

(** Applies the function to each member of the list and ands the results together. *)
let rec and_map : ('a -> bool) -> 'a list -> bool =
  fun f xs -> match xs with
    | [] -> true
    | (x::xs) -> f x && and_map f xs