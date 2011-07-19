open Batteries
open SortedFol
open PolicySyntax

module Ovs =
  struct
    type t = Set.StringSet.t Map.StringMap.t
    (** Empty *)
    let empty = Map.StringMap.empty
    (** Gets the higher-priority rules. Empty set if not existing. *)
    let get : t -> string -> Set.StringSet.t =
      fun ovs low ->
        try Map.StringMap.find low ovs
        with Not_found -> Set.StringSet.empty
    (** Adds that the higher priority overrides the lower. *)
    let add : t -> string -> string -> t =
      fun ovs low high ->
        Map.StringMap.add low (Set.StringSet.add high (get ovs low)) ovs
    (** Adds that the higher priorities override the lower. *)
    let add_all : t -> string -> string list -> t =
      fun ovs low highs ->
        List.fold_left (fun ovs high -> add ovs low high) ovs highs
    (** Finds the higher priority rules. *)
    let find : t -> string -> Set.StringSet.t =
      fun ovs low -> Map.StringMap.find low ovs
  end

(******************************************************************************)
(* General tools for computing the Ovs *)
(** Extract the names of a list of rules. *)
let rules2names rules = List.map (fun r -> r.rule_name) rules

(** Finds all of the rules with the specified decision in the policy. *)
let decision2rules : policy -> string -> string list =
  fun pol decision ->
    let correct_dec = List.filter (fun r -> r.rule_decision = decision)
                                  pol.rules in
    rules2names correct_dec
(* END General tools for computing the Ovs *)
(******************************************************************************)


(******************************************************************************)
(** Finds all of the rules that come before the low_rule in the policy. *)
let come_before : policy -> string -> rule list =
  fun pol low_rule ->
    List.take_while (fun r -> r.rule_name <> low_rule) pol.rules

(** Finds rules that come before the low_rule and are of the given decision *)
let find_higher_rules : policy -> string -> string -> string list =
  fun pol low_rule high_dec ->
    let correct_dec = List.filter (fun r -> r.rule_decision = high_dec)
                                  (come_before pol low_rule) in
    rules2names correct_dec
    
(* Turn FA declarations into Ovs *)
let handle_fa'' : policy -> string -> Ovs.t -> string -> Ovs.t =
  fun pol high_dec ovs low_rule ->
    let high_rules = find_higher_rules pol low_rule high_dec in
    List.fold_left (fun ovs high_rule -> Ovs.add ovs low_rule high_rule)
                   ovs high_rules

(** Turns a first applicable declaration into an Ovs.t *)
let handle_fa' : Ovs.t -> policy -> string -> string -> Ovs.t =
  fun ovs pol low_d high_d ->
    let low_rules = decision2rules pol low_d in
    List.fold_left (handle_fa'' pol high_d) ovs low_rules

let handle_fas' : Ovs.t -> policy -> string -> string list -> Ovs.t =
  fun ovs pol low_d high_ds ->
    List.fold_left (fun ovs high_dec -> handle_fa' ovs pol low_d high_dec) ovs high_ds

let handle_fas : Ovs.t -> policy -> string list -> Ovs.t =
  fun ovs pol decs ->
    List.fold_left (fun ovs low_dec -> handle_fas' ovs pol low_dec decs) ovs decs
(* END Turn FA declarations into Ovs *)
(******************************************************************************)
