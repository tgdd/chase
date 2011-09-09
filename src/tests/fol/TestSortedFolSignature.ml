(** Module for sorted first order logic type definitions and utilities.

    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)
open OUnit
open SortedFol
open Syntax
open Signature

let test_free_vars = "free_vars" >:: ( fun () ->
  assert_equal ["x"; "y"; "y"; "z"; "z"]
               (free_vars (string2formula 
                  "(exists xx A (and (P xx) (P x)
                                     (forall x B (or (R y) (R f(x y))))
                                     (P g(z z))))")))

let test_is_sentence = "is_sentence" >:::
[ "yes" >:: (fun () ->
    assert_bool "" (is_sentence (string2formula "(exists x A (forall y B (P x y)))"))
  )
; "no" >:: (fun () ->
    assert_bool "" (not (is_sentence (string2formula "(exists x A (forall y B (P x y z)))")))
  )
]


(** Determines if a formula is well-formed. The formula is well formed if
    - all predicates, functions, sorts and free variables appear in the
      signature, and
	  - the formula is well-sorted
val well_formed : sig_t -> (var_t * sort_t) list -> formula -> bool *)
let sgn =
  let sgn = Signature.init ["A"; "B"; "C"]
                           ["C","A"; "C","B"] in
  let sgn = Signature.add_func sgn "f" ["B"] "B" in
  let sgn = Signature.add_pred sgn "R" ["A"] in
  let sgn = Signature.add_pred sgn "P" ["A"; "B"] in
  sgn

let test_well_formed = "well_formed" >:::
[ "true" >:: (fun () ->
  assert_bool "" (well_formed sgn ["x","A";"y","B"]
                  (string2formula "(and (R x) (exists z B (P x f(z))))")))
; "false-freevars" >:: (fun () ->
  assert_bool "" (not (well_formed sgn []
                        (string2formula "(and (R x) (exists z B (P x f(z))))"))))
; "false-sig" >:: (fun () ->
  assert_bool "" (not (well_formed sgn ["x","A";"y","B"]
                        (string2formula "(and (Q x) (exists z B (P x f(z))))"))))
; "false-type" >:: (fun () ->
  assert_bool "" (not (well_formed sgn ["x","A";"y","B"]
                        (string2formula "(and (R y) (exists z B (P x f(z))))"))))
]

let tests = "SortedFolSignature" >::: [ test_free_vars
                                      ; test_is_sentence
                                      ; test_well_formed
                                      ]