type intorfloat = | Int of int | Float of float

type exprC = | NumC of intorfloat | IdC of string | StringC of string 
| AppC of exprC * exprC list | IfC of exprC * exprC * exprC | LamC of string list * exprC

(* value and binding depend on each other so use "and" *)
type value = | NumV of intorfloat | StringV of string | 
CloV of string list * exprC * binding list
| BoolV of bool | PrimOpV of string
and binding = Binding of string * value

let top_env = 
[Binding ("true", BoolV true); 
Binding ("false", BoolV false); 
Binding ("+", PrimOpV "+");
Binding ("-", PrimOpV "-");
Binding ("*", PrimOpV "*");
Binding ("/", PrimOpV "/");
Binding ("<=", PrimOpV "<=");
Binding ("equal?", PrimOpV "equal?");
Binding ("println", PrimOpV "println");
Binding ("read-num", PrimOpV "read-num");
Binding ("read-str", PrimOpV "read-str");
Binding ("seq", PrimOpV "seq");
Binding ("++", PrimOpV "++")]

(* todo: read-num, read-str, seq, ++, test cases for CloV and println *)
(* Maybe we can use this library for parsing? https://github.com/janestreet/sexplib *)

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

let rec extend_env params args env = 
  (match params with
  | [] -> env
  | p_head :: p_tail -> 
    (match args with 
      | a_head :: a_tail -> Binding (p_head, a_head) :: (extend_env p_tail a_tail env)
      | _ ->  raise (Invalid_argument "Invalid arg length list for CloV")))

let rec last_val_of_list lst =
  match lst with
  | [] -> failwith "List is empty"
  | [x] -> x
  | _ :: rest -> last_val_of_list rest

let read_number () =
  let input = read_line () in
  try
    let int_value = int_of_string input in
    NumV (Int int_value)
  with
  | Failure _ -> (
      try
        let float_value = float_of_string input in
        NumV (Float float_value)
      with
      | Failure _ ->
          raise (Invalid_argument "Invalid number as input")
    )

let rec interp env expr : value = 
  begin
  match expr with
  | NumC n -> NumV n
  | IdC s -> lookup env s
  | StringC str -> StringV str
  | LamC (params, expr) -> CloV (params, expr, env)
  | AppC (func, args) -> 
    begin
    match (interp env func) with 
    | PrimOpV op ->
      let interped_args = map_with_args interp env args in
      begin
      match interped_args with
        | [] ->
          begin
            match op with
            | "read-num" -> read_number ()
            | "read-str" -> StringV (read_line ())
            | _ -> raise (Invalid_argument "Invalid binop for no args")
          end
        | [NumV (Int n1); NumV (Int n2)] -> 
          begin
          match op with
          | "+" -> NumV (Int (n1 + n2)) 
          | "-" -> NumV (Int (n1 - n2))
          | "*" -> NumV (Int (n1 * n2))
          | "/" -> 
            if n2 = 0
            then raise (Invalid_argument "Division by zero error")
            else NumV (Int (n1 / n2))
          | "<=" -> BoolV (n1 <= n2)
          | "equal?" -> BoolV (n1 = n2)
          | "++" -> StringV (string_of_int n1 ^ string_of_int n2)
          | _ ->  raise (Invalid_argument "Invalid binop op for 2 ints")
          end
        | [NumV (Float n1); NumV (Float n2)] ->
          begin
          match op with
          | "+" -> NumV (Float (n1 +. n2))
          | "*" -> NumV (Float (n1 *. n2))
          | "-" -> NumV (Float (n1 -. n2))
          | "/" -> 
            if n2 = 0.0
            then raise (Invalid_argument "Division by zero error")
            else NumV (Float (n1 /. n2))
          | "<=" -> BoolV (n1 <= n2)
          | "equal?" -> BoolV (n1 = n2)
          | "++" -> StringV (string_of_float n1 ^ string_of_float n2)
          | _ -> raise (Invalid_argument "Invalid binop op for 2 floats")
          end
        | [NumV (Int n1); NumV (Float n2)] ->
          begin
            match op with
            | "++" -> StringV (string_of_int n1 ^ string_of_float n2)
            | _ -> raise (Invalid_argument "Invalid operation for types int and float")
          end
        | [NumV (Float n1); NumV (Int n2)] ->
          begin
            match op with
            | "++" -> StringV (string_of_float n1 ^ string_of_int n2)
            | _ -> raise (Invalid_argument "Invalid operation for types float and int")
          end
        | [StringV s1; StringV s2] ->
          begin
            match op with
            | "++" -> StringV (s1 ^ s2)
            | _ -> raise (Invalid_argument "Invalid operation for two strings")
          end
        | [StringV s] ->
          begin
          match op with
          | "println" -> 
            let () = (print_endline s) in 
            BoolV true
          | _ -> raise (Invalid_argument "Invalid PrimOpV with arg of one string.")
          end
        | _ ->
          begin
          match op with
          | "seq" -> last_val_of_list interped_args
          | _ -> raise (Invalid_argument "Invalid arg list for PrimOpV")
          end
      end
    | CloV (params, c_body, e) -> 
      let interped_args = map_with_args interp env args in
      interp (extend_env params interped_args e) c_body
    | _ -> raise (Invalid_argument "Invalid function")
    end
  | IfC (cond, thn, els) -> 
    (match (interp env cond) with
      | BoolV true -> interp env thn
      | BoolV false -> interp env els
      | _ -> raise (Invalid_argument "Given condition does not evaluate to boolean value"))
  end
      
(* To run: *)
(* opam install ounit2 *)
(* dune build tests.exe *)
(* dune exec ./tests.exe *)


