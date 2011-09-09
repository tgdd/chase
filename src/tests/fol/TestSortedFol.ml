open OUnit
open SortedFol
open Syntax

let test_string2formula = "string2formula" >:::
[ "Atom" >:: ( fun () ->
    assert_equal (Atom("P", [Var "x"]))
                 (string2formula "(P x)") )
; "Atom,FunApp" >:: ( fun () ->
    assert_equal (Atom("P", [ FunApp("f",[Var "x"])
                            ; FunApp("c",[])
                            ; FunApp("f",[FunApp("g", [ Var "y"
                                                      ; Var "x" ])])]))
                   (string2formula "(P f(x) 'c f(g(y x)))"))
; "Exists" >:: ( fun () ->
    assert_equal (Exists("x", "A",
                         (And [ Or []
                              ; And []
                              ; Forall("y", "B", (Or []))
                              ])))
                 (string2formula ( "(exists x A \n"
                                 ^ " (and (or) (and)\n (forall y B (or))))")))
; "Iff" >:: ( fun () ->
    assert_equal (Iff(fals, (Implies (fals, (And [Atom("P", [Var "x"])])))))
                 (string2formula ( "(iff false (-> false (and (P x))))")))
]

let test_string2theory = "string2theory" >:::
[ "Atom" >:: ( fun () ->
    assert_equal ([Syntax.Atom("P", [Syntax.Var "x"])])
                 (string2theory "(P x)") )
; "Atom2" >:: (fun () ->
    assert_equal ([ Syntax.Atom("P", [Syntax.Var "x"])
                  ; Syntax.Atom("P", [ FunApp("f",[Var "x"])
                                     ; FunApp("c",[])
                                     ; FunApp("f",[FunApp("g", [ Var "y"
                                                               ; Var "x" ])])])
                  ; Syntax.Atom("R",[Var "pasfd"])])
                  (string2theory "(P x)  \n (P f(x) 'c f(g(y x))) (R pasfd)"))
]

let tests = "TestSortedFol" >::: [test_string2formula; test_string2theory]