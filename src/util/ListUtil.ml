(* List utilities *)

(** Determines if a list is empty. More readable than "= []" *)
let is_empty xs = xs = []

(** Gets all but the last element of a list. *)
let rec initial xs = match xs with
  | [x] -> []
  | x::xs -> x :: (initial xs)
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


let rec find_duplicate xs = match xs with
  | [] -> None
  | x::xs -> if List.mem x xs then Some x else find_duplicate xs

let rec take_while f xs = match xs with
  | x::xs when f x -> x :: (take_while f xs)
  | _ -> []

let rec drop_until f xs = match xs with
  | x :: xs when not (f x) -> drop_until f xs
  | _ -> xs

let remove x xs = List.filter (fun y -> x <> y) xs