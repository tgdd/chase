(* List utilities *)
open Batteries

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
  fun f xs -> List.flatten (List.map f xs)

let rec find_duplicate xs = match xs with
  | [] -> None
  | x::xs -> if List.mem x xs then Some x else find_duplicate xs

let rec take_while f xs = match xs with
  | x::xs when f x -> x :: (take_while f xs)
  | _ -> []

let rec drop_until f xs = match xs with
  | x :: xs when not (f x) -> drop_until f xs
  | _ -> xs

let rec drop_through f xs = match xs with
  | [] -> []
  | x :: xs when f x -> xs
  | x :: xs -> drop_until f xs

let remove x xs = List.filter (fun y -> x <> y) 


let cartesian_product xs ys =
  map_append (fun a -> List.map (fun b -> (a, b)) ys) xs

let relation2function xs =
  let keys = List.unique (List.map fst xs)  in
  let vals key xs = List.filter_map (fun x -> if key = fst x
                                              then Some (snd x)
                                              else None)
                                    xs in
  List.map (fun key -> (key, vals key xs)) keys