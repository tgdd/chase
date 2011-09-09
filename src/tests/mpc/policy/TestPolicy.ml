open OUnit
module P = Policy
module PS = P.Syntax
module PC = P.Compiler
module V = Vocab
module VS = V.Syntax
module S = SortedFol.Signature.Signature

(* Set up sig for all tests *)
let sgn = 
  let sgn = S.init ["Subject"; "Action"; "Resource"; "Student"; "Professor"]
                   ["Subject", "Student"; "Subject", "Professor"] in
  let sgn = S.add_pred sgn "permit" ["Subject"; "Action"; "Resource"] in
  let sgn = S.add_pred sgn "deny" ["Subject"; "Action"; "Resource"] in
  let sgn = S.add_pred sgn "log" ["Subject"; "Action"; "Resource"] in
  let sgn = S.add_func sgn "dan" [] "Professor" in
  let sgn = S.add_func sgn "tim" [] "Student" in
  let sgn = S.add_func sgn "advisor" ["Student"] "Professor" in
  sgn

(* Policy for first two tests *)
let pol1 =
  { PS.uses = "example.vocab"
  ; PS.target = SortedFol.string2formula "(forall a Student (exists b Student (a = b)))"
  ; PS.vars = [ { PS.variable_name = "s"
                ; PS.variable_sort = "Student"
                }
              ; { PS.variable_name = "p"
                ; PS.variable_sort = "Professor"
                }
              ; { PS.variable_name = "a"
                ; PS.variable_sort = "Action"
                }
              ; { PS.variable_name = "r"
                ; PS.variable_sort = "Resource"
                }]
  ; PS.rules = [ { PS.rule_name = "rule1"
                 ; PS.rule_decision = "permit"
                 ; PS.rule_parameters = ["s"; "a"; "r"]
                 ; PS.rule_body = (SortedFol.string2formula "(exists adv Professor (adv = advisor(s)))")
                 }
               ; { PS.rule_name = "rule2"
                 ; PS.rule_decision = "deny"
                 ; PS.rule_parameters = ["s"; "a"; "r"]
                 ; PS.rule_body = (SortedFol.string2formula "(and (exists adv Professor (adv = advisor(s)))
                                                                  (not (adv = p)))")
                 }
               ]
  ; PS.dcrds = [ PS.Overrides("permit", ["deny"])]
  }

(* Text equivalent of pol1 for parsing test *)
let pol_text1 =
"(Policy uses example.vocab
   (Target (forall a Student (exists b Student (a = b))))
   (Variables (Variable s Student)
              (Variable p Professor)
              (Variable a Action)
              (Variable r Resource))
   (Rules (rule1 = (permit s a r) :-
                   (exists adv Professor (adv = advisor(s))))
          (rule2 = (deny s a r) :-
                   (exists adv Professor (adv = advisor(s)))
                   (not (adv = p))))
   (RComb (Overrides permit deny)))"


let expected_sgn1 =
  let sgn = S.add_pred sgn "rule1_{matches}" ["Subject"; "Action"; "Resource"] in
  let sgn = S.add_pred sgn "rule1_{applies}" ["Subject"; "Action"; "Resource"] in
  let sgn = S.add_pred sgn "rule2_{matches}" ["Subject"; "Action"; "Resource"] in
  let sgn = S.add_pred sgn "rule2_{applies}" ["Subject"; "Action"; "Resource"] in
  sgn

let expected_thy1 =
  SortedFol.string2theory
  "(iff (rule1_{matches} s a r) (and (forall a Student (exists b Student (a = b)))
                                   (exists adv Professor (adv = advisor(s)))))
   (iff (rule2_{matches} s a r) (and (forall a Student (exists b Student (a = b)))
                                   (and (exists adv Professor (adv = advisor(s)))
                                        (not (adv = p)))))
   (iff (rule1_{applies} s a r) (and (rule1_{matches} s a r) 
                                     (not (or (rule2_{matches} s a r)))))
   (iff (rule2_{applies} s a r) (and (rule2_{matches} s a r)
                                     (not (or))))
   (iff (permit s a r) (or (rule1_{applies} s a r)))
   (iff (deny s a r) (or (rule2_{applies} s a r)))"

(* policy for 3rd test *)

let pol2 = P.string2policy
"(Policy uses example.vocab
   (Target (forall a Student (exists b Student (a = b))))
   (Variables (Variable s Student)
              (Variable p Professor)
              (Variable a Action)
              (Variable r Resource))
   (Rules (rule1 = (permit s a r) :- (s = r))
          (rule2 = (deny s a r) :- (not (s = r)))
          (rule3 = (log s a r) :- (not (s = r)))
          (rule4 = (permit s a r) :- (a = a)))
   (RComb (FA permit deny)
          (Overrides log permit deny)))"

let expected_sgn2 =
  let sgn = S.add_pred sgn "rule1_{matches}" ["Subject"; "Action"; "Resource"] in
  let sgn = S.add_pred sgn "rule1_{applies}" ["Subject"; "Action"; "Resource"] in
  let sgn = S.add_pred sgn "rule2_{matches}" ["Subject"; "Action"; "Resource"] in
  let sgn = S.add_pred sgn "rule2_{applies}" ["Subject"; "Action"; "Resource"] in
  let sgn = S.add_pred sgn "rule3_{matches}" ["Subject"; "Action"; "Resource"] in
  let sgn = S.add_pred sgn "rule3_{applies}" ["Subject"; "Action"; "Resource"] in
  let sgn = S.add_pred sgn "rule4_{matches}" ["Subject"; "Action"; "Resource"] in
  let sgn = S.add_pred sgn "rule4_{applies}" ["Subject"; "Action"; "Resource"] in
  sgn

let expected_thy2 =
  SortedFol.string2theory
  "(iff (rule1_{matches} s a r) (and (forall a Student (exists b Student (a = b)))
                                   (s = r)))
   (iff (rule2_{matches} s a r) (and (forall a Student (exists b Student (a = b)))
                                   (not (s = r))))
   (iff (rule3_{matches} s a r) (and (forall a Student (exists b Student (a = b)))
                                   (not (s = r))))
   (iff (rule4_{matches} s a r) (and (forall a Student (exists b Student (a = b)))
                                   (a = a)))
   (iff (rule1_{applies} s a r) (and (rule1_{matches} s a r) (not (or))))
   (iff (rule2_{applies} s a r) (and (rule2_{matches} s a r)
                                   (not (or (rule1_{matches} s a r)))))
   (iff (rule3_{applies} s a r) (and (rule3_{matches} s a r)
                                   (not (or (rule1_{matches} s a r)
                                            (rule2_{matches} s a r)
                                            (rule4_{matches} s a r)))))
   (iff (rule4_{applies} s a r) (and (rule4_{matches} s a r)
                                   (not (or (rule1_{matches} s a r)
                                            (rule2_{matches} s a r)))))
   (iff (deny s a r) (or (rule2_{applies} s a r)))
   (iff (log s a r) (or (rule3_{applies} s a r)))
   (iff (permit s a r) (or (rule1_{applies} s a r)
                           (rule4_{applies} s a r)))"

let test_string2policy = "string2policy" >::
  ( fun () ->
      assert_equal ~printer:P.show_policy pol1 (P.string2policy pol_text1)
  )
(* assertion helpers *)
let assert_equal_signature =
  assert_equal ~printer:S.show 
let assert_equal_theory =
  assert_equal ~printer:(fun thy -> StringUtil.unlines (List.map SortedFol.Syntax.show_formula thy))
let test_compile_policy = "compile_policy" >:::
  [ "pol1sgn" >::
    ( fun () ->
      let sgn, thy = PC.compile_policy sgn pol1 in
      assert_equal_signature expected_sgn1 sgn
    )
  ; "pol1thy" >::
    ( fun () ->
      let sgn, thy = PC.compile_policy sgn pol1 in
      assert_equal_theory expected_thy1 thy
    )
  ; "pol2sng" >::
     ( fun () ->
        let sgn, thy = PC.compile_policy sgn pol2 in
        assert_equal_signature expected_sgn2 sgn
        
     )
  ; "pol2thy" >::
     ( fun () ->
        let sgn, thy = PC.compile_policy sgn pol2 in
        assert_equal_theory expected_thy2 thy
     )
  ]

let tests = "TestPolicy" >::: [ test_string2policy; test_compile_policy ]
