(* Name: Gautam Gupta

   UID: 304282688

   Others With Whom I Discussed Things: Kelly Hosokawa, Jennifer Tan, David Pu

   Other Resources I Consulted: N/A

*)

(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
   You should provide a useful error message.
*)
exception DynamicTypeError of string

(* This exception is thrown when pattern matching fails during evaluation. *)
exception MatchFailure

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
  match (pat, value) with
  (* an integer pattern matches an integer only when they are the same constant;
     no variables are declared in the pattern so the returned environment is empty *)
    (IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
  | (BoolPat(i), BoolVal(j)) when i=j -> Env.empty_env()
  | (DataPat(si, None) , DataVal(sj, None)) when si = sj -> Env.empty_env()
  | (DataPat(si, Some p), DataVal(sj, Some v)) when si=sj -> patMatch p v
  | (TuplePat(l1), TupleVal(l2)) when List.length l1 = List.length l2 ->
      List.fold_right2 (fun elem1 elem2 rest ->
          Env.combine_envs rest (patMatch elem1 elem2)) l1 l2 (Env.empty_env())
  | (VarPat(str), _) -> Env.add_binding str value (Env.empty_env())
  | (WildcardPat, _) -> Env.empty_env()
  | _ -> raise MatchFailure


(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)
let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
  (* an integer constant evaluates to itself *)
    IntConst(i) -> IntVal(i)
  | BoolConst(b) -> BoolVal(b)
  | Var(s) -> (try Env.lookup s env
               with Env.NotBound ->
                 raise (
                   DynamicTypeError ("Unbound value "^ s)))
  | BinOp(lexp, op, rexp) ->
      (match ((evalExpr lexp env), (evalExpr rexp env)) with
         (IntVal(l), IntVal(r)) ->
           (match op with
              Plus -> IntVal (l+r)
            | Minus -> IntVal (l-r)
            | Times -> IntVal (l*r)
            | Eq -> BoolVal(l=r)
            | Gt -> BoolVal(l>r)
           )
       | (_, _) -> raise (DynamicTypeError "Binary operator expects an \
                                            expression of type int")
      )
  | Negate(exp) ->
      (match (evalExpr exp env) with
         IntVal(i) -> IntVal(-i)
       | _ -> raise (DynamicTypeError "Negation expects an expression of type \
                                       int")
      )
  | If(cond, thenexp, elseexp) ->
      (match (evalExpr cond env) with
         BoolVal(b) -> evalExpr (if b then thenexp else elseexp) env
       | _ -> raise (DynamicTypeError "If condition must evaluate to boolean")
      )
  | Function(pat, expr) -> FunctionVal(None, pat, expr, env)
  | FunctionCall(lexp, rexp) ->
      (match evalExpr lexp env with
         FunctionVal(None, pat, exp, fenv) ->
           let matchedEnv = patMatch pat (evalExpr rexp env) in
           evalExpr exp (Env.combine_envs fenv matchedEnv)
       | FunctionVal(Some str, pat, exp, fenv) ->
           let matchedEnv = patMatch pat (evalExpr rexp env) in
           let fenv =
             (Env.add_binding str (FunctionVal(Some str, pat, exp, fenv)) fenv)
           in
           evalExpr exp (Env.combine_envs fenv matchedEnv)
       | _ -> raise (DynamicTypeError "Only functions are callable")
      )
  | Match(expr, mlist) -> (match mlist with
        (pat, rexpr)::t ->
          (try
             let matched_env = patMatch pat (evalExpr expr env) in
             evalExpr rexpr (Env.combine_envs env matched_env) with
           | MatchFailure -> evalExpr (Match(expr, t)) env
          )
      | [] -> raise MatchFailure
    )
  | Tuple(l) -> TupleVal(List.map (fun expr -> evalExpr expr env) l)
  | Data(str, expr) -> (match expr with
        Some exp -> DataVal(str, (Some (evalExpr exp env)))
      | _ -> DataVal(str, None)
    )

(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
  (* a top-level expression has no name and is evaluated to a value *)
    Expr(e) -> (None, evalExpr e env)
  | Let(str, expr) -> (Some str, evalExpr expr env)
  | LetRec(str, Function(pat, expr)) -> (Some str,
                                         FunctionVal(Some str, pat, expr, env))
  | _ -> raise (DynamicTypeError "Invalid declaration type")

