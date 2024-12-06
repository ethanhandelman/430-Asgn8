open OUnit2
open Asgn8
open Unix
(* opam install ounit2 *)
(* Run "ocamlfind ocamlc -o test -package oUnit2 -linkpkg asgn8.ml", and then run "./test" *)
  
let test_print _ =
  let old_stdout = dup stdout in  (* Save the current stdout *)
  let pipe_r, pipe_w = pipe () in  (* Create a pipe to capture the output *)

  dup2 pipe_w stdout;

  let _ = interp top_env (AppC ((IdC "println"), [StringC "Hello, World!"])) in
  close pipe_w;
  let output = input_line (in_channel_of_descr pipe_r) in
  assert_equal "Hello, World!" output;

  (* Restore the original stdout *)
  dup2 old_stdout stdout;
  close pipe_r;
  close old_stdout


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

  "if2" >:: (fun _ -> assert_equal (interp top_env (IfC ((IdC "false"), (NumC (Int 1)), (NumC (Int 0))))) (NumV (Int 0)));

  "iferr" >:: (fun _ -> assert_raises (Invalid_argument "Given condition does not evaluate to boolean value")
  (fun () -> (interp top_env (IfC ((NumC (Int 3)), (NumC (Int 1)), (NumC (Int 0)))))));

  "lam1" >:: (fun _ -> assert_equal (interp top_env (LamC (["a"; "b"], (NumC (Int 1))))) (CloV (["a"; "b"], (NumC (Int 1)), top_env)));

  "plusplus1" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "++"),
  [StringC "aaa"; StringC "bbb"]))) (StringV "aaabbb"));

  "plusplus2" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "++"),
  [NumC (Int 3); NumC (Int 4)]))) (StringV "34"));

  "plusplus3" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "++"),
  [NumC (Int 3); NumC (Float 4.0)]))) (StringV "34."));

  "test-seq" >:: (fun _ -> assert_equal (interp top_env (AppC ((IdC "seq"), [(NumC (Int 1)); (NumC (Int 2)); (NumC (Int 3))]))) (NumV (Int 3)));

  "test-println" >:: test_print;

  "test-clov" >:: (fun _ -> assert_equal (interp top_env (AppC ((LamC (["a"; "b"], (NumC (Int 1)))), [(NumC (Int 2)); (NumC (Int 3))]))) (NumV (Int 1)));

]

let _ = run_test_tt_main tests

