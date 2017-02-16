(* vim: set ft=ocaml tabstop=2 shiftwidth=2 commentstring=(*\ %s\ *): *)
(* 
   CS131 Homework 2 - Gautam Gupta #304282688 
   Resources used: None
*)
exception ImplementMe

(* Problem 1: Vectors and Matrices *)

(* type aliases for vectors and matrices *)
type vector = float list
type matrix = vector list

let (vplus : vector -> vector -> vector) = fun v1 v2 ->
  List.map2 (+.) v1 v2

let (mplus : matrix -> matrix -> matrix) = fun m1 m2 ->
  List.map2 vplus m1 m2

let (dotprod : vector -> vector -> float) = fun v1 v2 ->
  List.fold_right2 (fun val1 val2 sum -> val1 *. val2 +. sum) v1 v2 0.0

let (getNthRow : matrix -> int -> vector) = fun mat i ->
  List.nth mat i

let (getNthCol : matrix -> int -> vector) = fun mat i ->
  List.map (fun v -> List.nth v i) mat

let (transpose : matrix -> matrix) = fun mat ->
  if mat = [] then
    []
  else
    (List.mapi (fun i el -> getNthCol mat i) (List.hd mat))

let (mmult : matrix -> matrix -> matrix) = fun mat1 mat2 ->
  let tmat2 = (transpose mat2) in
    List.map (fun row -> 
      List.map (fun col -> dotprod row col) tmat2) mat1

(* Problem 2: Calculators *)

(* a type for arithmetic expressions *)
type op = Plus | Minus | Times | Divide
type exp = Num of float | BinOp of exp * op * exp

let rec (evalExp : exp -> float) = function
  | BinOp (lhs, Plus, rhs) -> (evalExp lhs) +. (evalExp rhs)
  | BinOp (lhs, Minus, rhs) -> (evalExp lhs) -. (evalExp rhs)
  | BinOp (lhs, Times, rhs) -> (evalExp lhs) *. (evalExp rhs)
  | BinOp (lhs, Divide, rhs) -> (evalExp lhs) /. (evalExp rhs)
  | Num n -> n

(* a type for stack instructions *)
type instr = Push of float | Swap | Calculate of op

let (execute : instr list -> float) = fun instructions ->
  let rec (executeHelper : instr list -> float list -> float) =
    fun ins stack ->
      match ins with
      | Push f::t -> executeHelper t (f::stack)
      | Swap::t -> let n1::n2::stackt = stack in
          (executeHelper t (n2::n1::stackt))
      | Calculate oper::t -> let n1::n2::stackt = stack in
          executeHelper t ((evalExp (BinOp(Num n2, oper, Num n1)))::stackt)
      | [] -> List.hd stack
  in executeHelper instructions []

let rec (compile : exp -> instr list) = function
  | Num f -> [Push f]
  | BinOp(lhs, oper, rhs) -> (compile lhs)@(compile rhs)@[Calculate oper]

let (decompile : instr list -> exp) = fun instructions ->
  let rec (decompileHelper : instr list -> exp list -> exp) = fun ins stack ->
    match ins with
    | Push f::t -> decompileHelper t ((Num f)::stack)
    | Swap::t -> let n1::n2::stackt = stack in 
        decompileHelper t (n2::n1::stackt)
    | Calculate oper::t -> let n1::n2::stackt = stack in 
        decompileHelper t ((BinOp(n2, oper, n1))::stackt)
    | [] -> List.hd stack
  in decompileHelper instructions []
    

(* EXTRA CREDIT *)

let rec (compileOpt : exp -> (instr list * int)) = function
  | Num f -> ([Push f], 1)
  | BinOp(lexp, oper, rexp) ->
      let (lcomp, lsize) = compileOpt lexp in
      let (rcomp, rsize) = compileOpt rexp in
      let lmax = max lsize (rsize + 1) in
      let rmax = max rsize (lsize + 1) in
      if lmax <= rmax then 
        lcomp@rcomp@[Calculate oper], lmax
      else
        if oper = Divide || oper = Minus then
          rcomp@lcomp@[Swap; Calculate oper], rmax
        else
          rcomp@lcomp@[Calculate oper], rmax
