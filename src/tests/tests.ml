open OUnit 

let tests = "Margrave" >::: [ TestSortedFol.tests
                            ; TestSortedFolSignature.tests
                            ; TestVocab.tests
                            ; TestPolicy.tests
                            ]
let _ = run_test_tt_main tests