open OUnit
open Vocab
open Syntax
module S = SortedFol.Signature.Signature

let assert_equal_vocs = assert_equal ~printer:show_vocab

let voc =
  { vocab_name = "vname"
  ; sorts = [ { sort_name = "Subject"; subsorts = ["Student"; "Professor"] }
            ; { sort_name = "Action"; subsorts = [] }
            ; { sort_name = "Resource"; subsorts = [] }
            ]
  ; preds = [ { pred_name = "permit"; pred_arity = ["Subject"; "Action"; "Resource"] }
            ; { pred_name = "deny"; pred_arity = ["Subject"; "Action"; "Resource"] }
            ]
  ; consts = [ { const_name = "dan"; const_sort = "Professor" }
             ; { const_name = "tim"; const_sort = "Student" }
             ]
  ; funcs = [ { func_name = "advisor"; func_arity = ["Student"]; func_sort = "Professor" }
  ] }
let test_string2vocab = "string2vocab" >::
 ( fun () ->
    assert_equal_vocs voc
                 (string2vocab
                  ( "(Vocab vname"
                  ^ " (Types (Type Subject > Student Professor)"
                  ^ " (Type Action)"
                  ^ " (Type Resource))"
                  ^ " (Predicates (Predicate permit Subject Action Resource)"
                  ^ " (Predicate deny Subject Action Resource)) "
                  ^ " (Constants (Constant dan Professor) (Constant tim Student))"
                  ^ " (Functions (Function advisor Student Professor)))"
                  )))
                  

let assert_equal_sigs = assert_equal ~cmp:S.equal ~printer:S.show
let sgn = 
  let sgn = S.init ["Subject"; "Action"; "Resource"; "Student"; "Professor"]
                   ["Subject", "Student"; "Subject", "Professor"] in
  let sgn = S.add_pred sgn "permit" ["Subject"; "Action"; "Resource"] in
  let sgn = S.add_pred sgn "deny" ["Subject"; "Action"; "Resource"] in
  let sgn = S.add_func sgn "dan" [] "Professor" in
  let sgn = S.add_func sgn "tim" [] "Student" in
  let sgn = S.add_func sgn "advisor" ["Student"] "Professor" in
  sgn

let test_vocab2signature = "vocab2signature" >::
  ( fun () ->
      assert_equal_sigs sgn (Compiler.vocab2signature voc)
  )

let tests = "TestVocab" >::: [ test_string2vocab; test_vocab2signature ]
