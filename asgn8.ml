type intorfloat = | Int of int | Float of float
type exprC = | NumC of intorfloat | IdC of string | StringC of string | AppC of exprC * exprC list | IfC of exprC * exprC * exprC
type value = | NumV of intorfloat | StringV of string | BoolV of bool
type binding = Binding of string * value

(* IMPORTANT: TO THE NEXT PERSON WORKING ON THIS, IfC needs more test cases and environment needs to be expanded *)

let top_env = [Binding ("true", BoolV true); Binding ("false", BoolV false)]

let rec map_with_args f arg lst =
  match lst with
  | [] -> []                   
  | x :: xs ->                 
      f arg x :: map_with_args f arg xs  

let rec lookup env id =
  match env with
  | [] -> raise (Invalid_argument ("Symbol not found within environment: " ^ id))
  | Binding (name, value) :: rest ->
      if name = id then value
      else lookup rest id

let rec interp env expr : value = 
  (match expr with
  | NumC n -> NumV n
  | IdC s -> lookup env s
  | StringC str -> StringV str
  | AppC (func, args) -> 
    let interped_args = map_with_args interp env args in
      (match interped_args with 
      | [NumV (Int n1); NumV (Int n2)] ->
        (match func with 
        | IdC "+" -> NumV (Int (n1 + n2))
        | IdC "*" -> NumV (Int (n1 * n2))
        | IdC "-" -> NumV (Int (n1 - n2))
        | IdC "/" -> 
          if n2 = 0
          then raise (Invalid_argument "Division by zero error")
          else NumV (Int (n1 / n2))
        | IdC "<=" -> BoolV (n1 <= n2)
        | IdC "equal?" -> BoolV (n1 = n2)
        | _ -> invalid_arg "Invalid function name")
      | [NumV (Float n1); NumV (Float n2)] ->
        (match func with 
        | IdC "+" -> NumV (Float (n1 +. n2))
        | IdC "*" -> NumV (Float (n1 *. n2))
        | IdC "-" -> NumV (Float (n1 -. n2))
        | IdC "/" -> 
          if n2 = 0.0
          then raise (Invalid_argument "Division by zero error")
          else NumV (Float (n1 /. n2))
        | IdC "<=" -> BoolV (n1 <= n2)
        | IdC "equal?" -> BoolV (n1 = n2)
        | _ -> raise (Invalid_argument "Invalid function name"))
      | _ -> raise (Invalid_argument "Invalid function name"))
  | IfC (cond, thn, els) -> 
    (match (interp env cond) with
      | BoolV true -> interp env thn
      | BoolV false -> interp env els
      | _ -> raise (Invalid_argument "Given condition does not evaluate to boolean value")))
      
(* To run: *)
(* opam install ounit2 *)
(* dune build tests.exe *)
(* dune exec ./tests.exe *)


