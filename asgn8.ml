type intorfloat = | Int of int | Float of float
type exprC = | NumC of intorfloat | IdC of string | StringC of string | AppC of exprC * exprC list
type value = | NumV of intorfloat | StringV of string | BoolV of bool

(* IMPORTANT: TO THE NEXT PERSON WORKING ON THIS, I did not finish 'equal PrimV and did not finish test cases *)

let interp expr : value = 
  match expr with
  | NumC n -> NumV n
  | IdC s -> StringV s
  | StringC str -> StringV str
  | AppC (func, args) -> 
    match args with 
    | [NumC (Int n1); NumC (Int n2)] ->
      (match func with 
      | IdC "+" -> NumV (Int (n1 + n2))
      | IdC "*" -> NumV (Int (n1 * n2))
      | IdC "-" -> NumV (Int (n1 - n2))
      | IdC "/" -> 
        if n2 = 0
        then raise (Invalid_argument "Division by zero error")
        else NumV (Int (n1 / n2))
      | IdC "<=" -> BoolV (n1 <= n2)
      | _ -> invalid_arg "Invalid function name")
    | [NumC (Float n1); NumC (Float n2)] ->
      (match func with 
      | IdC "+" -> NumV (Float (n1 +. n2))
      | IdC "*" -> NumV (Float (n1 *. n2))
      | IdC "-" -> NumV (Float (n1 -. n2))
      | IdC "/" -> 
        if n2 = 0.0
        then raise (Invalid_argument "Division by zero error")
        else NumV (Float (n1 /. n2))
      | IdC "<=" -> BoolV (n1 <= n2)
      | _ -> invalid_arg "Invalid function name")
    | _ -> invalid_arg "Invalid arguments"
      


open OUnit2
(* opam install ounit2 *)
(* Run "ocamlfind ocamlc -o test -package oUnit2 -linkpkg asgn8.ml", and then run "./test" *)


let tests = "test suite for interp" >::: [
  "num" >:: (fun _ -> assert_equal (interp (NumC (Int 5))) (NumV (Int 5)));
  "id" >:: (fun _ -> assert_equal (interp (IdC "hello")) (StringV "hello"));
  "string" >:: (fun _ -> assert_equal (interp (StringC "aaaa")) (StringV "aaaa"));
  "plus" >:: (fun _ -> assert_equal (interp (AppC ((IdC "+"), 
  [NumC (Int 2); NumC (Int 3)]))) (NumV (Int 5)));

  "plus2" >:: (fun _ -> assert_equal (interp (AppC ((IdC "+"), 
  [NumC (Float 2.5); NumC (Float 3.3)]))) (NumV (Float 5.8)));

  "mult" >:: (fun _ -> assert_equal (interp (AppC ((IdC "*"), 
  [NumC (Int 2); NumC (Int 2)]))) (NumV (Int 4)));

  "mult2" >:: (fun _ -> assert_equal (interp (AppC ((IdC "*"), 
  [NumC (Float 2.5); NumC (Float 2.5)]))) (NumV (Float 6.25)));

  "minus" >:: (fun _ -> assert_equal (interp (AppC ((IdC "-"), 
  [NumC (Int 5); NumC (Int 3)]))) (NumV (Int 2)));

  "minus2" >:: (fun _ -> assert_equal (interp (AppC ((IdC "-"), 
  [NumC (Int 2); NumC (Int 3)]))) (NumV (Int (-1))));

  "minus3" >:: (fun _ -> assert_equal (interp (AppC ((IdC "-"), 
  [NumC (Float 5.5); NumC (Float 3.5)]))) (NumV (Float 2.0)));

  "division" >:: (fun _ -> assert_equal (interp (AppC ((IdC "/"), 
  [NumC (Int 9); NumC (Int 3)]))) (NumV (Int 3)));

  "division2" >:: (fun _ -> assert_equal (interp (AppC ((IdC "/"),
   [NumC (Float 9.0); NumC (Float 4.5)]))) (NumV (Float 2.0)));

  "division3" >:: (fun _ -> assert_raises (Invalid_argument "Division by zero error")
  (fun () -> (interp (AppC ((IdC "/"), [NumC (Float 9.0); NumC (Float 0.0)])))));

  "division4" >:: (fun _ -> assert_raises (Invalid_argument "Division by zero error")
  (fun () -> (interp (AppC ((IdC "/"), [NumC (Int 9); NumC (Int 0)])))));

  "lessthan" >:: (fun _ -> assert_equal (interp (AppC ((IdC "<="),
  [NumC (Float 9.0); NumC (Float 4.5)]))) (BoolV false));

  "lessthan2" >:: (fun _ -> assert_equal (interp (AppC ((IdC "<="),
  [NumC (Float 3.0); NumC (Float 4.5)]))) (BoolV true));

  "lessthan3" >:: (fun _ -> assert_equal (interp (AppC ((IdC "<="),
  [NumC (Int 4); NumC (Int 4)]))) (BoolV true));

  "lessthan4" >:: (fun _ -> assert_equal (interp (AppC ((IdC "<="),
  [NumC (Int 5); NumC (Int 4)]))) (BoolV false));

]

let _ = run_test_tt_main tests




