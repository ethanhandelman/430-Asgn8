open OUnit2
open Asgn8
(* opam install ounit2 *)
(* Run "ocamlfind ocamlc -o test -package oUnit2 -linkpkg asgn8.ml", and then run "./test" *)


let tests = "test suite for interp top_env" >::: [
  "num" >:: (fun _ -> assert_equal (interp top_env (NumC (Int 5))) (NumV (Int 5)));
  "string" >:: (fun _ -> assert_equal (interp top_env (StringC "aaaa")) (StringV "aaaa"));
  "plus" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "+"), 
  [NumC (Int 2); NumC (Int 3)]))) (NumV (Int 5)));

  "plus2" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "+"), 
  [NumC (Float 2.5); NumC (Float 3.3)]))) (NumV (Float 5.8)));

  "mult" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "*"), 
  [NumC (Int 2); NumC (Int 2)]))) (NumV (Int 4)));

  "mult2" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "*"), 
  [NumC (Float 2.5); NumC (Float 2.5)]))) (NumV (Float 6.25)));

  "minus" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "-"), 
  [NumC (Int 5); NumC (Int 3)]))) (NumV (Int 2)));

  "minus2" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "-"), 
  [NumC (Int 2); NumC (Int 3)]))) (NumV (Int (-1))));

  "minus3" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "-"), 
  [NumC (Float 5.5); NumC (Float 3.5)]))) (NumV (Float 2.0)));

  "division" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "/"), 
  [NumC (Int 9); NumC (Int 3)]))) (NumV (Int 3)));

  "division2" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "/"),
  [NumC (Float 9.0); NumC (Float 4.5)]))) (NumV (Float 2.0)));

  "division3" >:: (fun _ -> assert_raises (Invalid_argument "Division by zero error")
  (fun () -> (interp top_env (AppC ((IdC "/"), [NumC (Float 9.0); NumC (Float 0.0)])))));

  "division4" >:: (fun _ -> assert_raises (Invalid_argument "Division by zero error")
  (fun () -> (interp top_env (AppC ((IdC "/"), [NumC (Int 9); NumC (Int 0)])))));

  "nestedplus" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "+"), 
  [AppC ((IdC "+"), [NumC (Int 2); NumC (Int 6)]); NumC (Int 3)]))) (NumV (Int 11)));

  "lessthan" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "<="),
  [NumC (Float 9.0); NumC (Float 4.5)]))) (BoolV false));

  "lessthan2" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "<="),
  [NumC (Float 3.0); NumC (Float 4.5)]))) (BoolV true));

  "lessthan3" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "<="),
  [NumC (Int 4); NumC (Int 4)]))) (BoolV true));

  "lessthan4" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "<="),
  [NumC (Int 5); NumC (Int 4)]))) (BoolV false));

  "equalints" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "equal?"),
  [NumC (Int 5); NumC (Int 5)]))) (BoolV true));

  "unequalints" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "equal?"),
  [NumC (Int 5); NumC (Int 6)]))) (BoolV false));

  "if1" >:: (fun _ -> assert_equal (interp top_env (IfC ((IdC "true"), (NumC (Int 1)), (NumC (Int 0))))) (NumV (Int 1)));

]

let _ = run_test_tt_main tests

