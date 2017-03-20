open Ha7_angabe
open Batteries.List
open Printf

(* Endrekursive Version des @-Operators *)
let (@!) l x =
  List.rev (x :: (List.rev l))

let hyperfib k n =
  let rec inner x l = (
    match l with
      _::tl ->
      let next = List.fold_left (+) 0 l in
      assert (next >= 0);
      if x == n then next
      else
        inner (x + 1) (tl @! next)
    | [] -> failwith "Invalid argument") in
  if n <= k then n
  else
    let l = init k (fun k -> k + 1) in
    inner (k + 1) l

(* Collatz Conjecture *)
let coco n =
  let rec inner count n =
    assert (count >= 0);
    assert (n >= 0);
    if n = 1 then count
    else inner (count + 1) (if n mod 2 = 0 then n/2
                            else 3*n + 1)
  in inner 1 n

(* Ein Typ für Continuations; der Konstruktor wird gebraucht,
   um Rekursion zu erlauben. *)
type cont = Cont of (int list -> cont list -> bin_tree -> int list)

let to_sorted_list t =
  let rec inner l conts = function
      Node n -> let cont = Cont (fun l conts t ->
        inner (n.key::l) conts n.right)
      in inner l (cont::conts) n.left
    | Leaf -> (match conts with
          (Cont hd)::tl -> hd l tl t
        | [] -> l
      )
  in List.rev (inner [] [] t)

type state = (variable * expression option) list
[@@deriving show]

type ocaml_evaluator =
    IntIntInt of (int -> int -> int)
  | IntIntBool of (int -> int -> bool)
  | BoolBoolBool of (bool -> bool -> bool)
  | IntInt of (int -> int)
  | BoolBool of (bool -> bool)

(* Bei Funktionsaufrufen werden die Formalen Parameter durch die
   tatsächlichen Parameter ersetzt. Diese Funktion ersetzt eine
   einzige Variable. *)
let substitute v exp e =
  let rec inner e = match e with
      Variable v' when v' = v -> exp
    | Variable v' -> e
    | Immediate i -> e
    | BinOperation (op, lhs, rhs) -> BinOperation (op, inner lhs, inner rhs)
    | UnOperation (op, operand) -> UnOperation (op, inner operand)
    | Binding b -> Binding {b with expr = inner b.expr; inner = inner b.inner}
    | Func f -> Func {f with body = inner f.body}
    | Closure c -> Closure {c with body = inner c.body}
    | FuncApp fa -> FuncApp { f = inner fa.f; param_act = inner fa.param_act }
    | IfThenElse ite -> IfThenElse { cond = inner ite.cond;
                                     then_branch = inner ite.then_branch;
                                     else_branch = inner ite.else_branch } in
  inner e

(* "Krücke" für Closures; da in der Angabe leider der Kontext bei Closures
   fehlte, werden Werte stattdessen Substituiert. Dies ist problematisch bei
   rekursiven Captures. *)
let substitute_from_state s e =
  let foo s v = let may_e = (try List.assoc v s with
        Not_found -> failwith (Printf.sprintf "Unbound variable '%s'" v.name)) in
    (match may_e with
       Some e -> e
     | None -> Variable v) in
  let rec inner s e = match e with
    | Variable v -> foo s v
    | Immediate i -> e
    | BinOperation (op, lhs, rhs) -> BinOperation (op, inner s lhs, inner s rhs)
    | UnOperation (op, operand) -> UnOperation (op, inner s operand)
    | Binding b -> let s' = (b.var, None)::s in
      Binding {b with expr = inner s' b.expr; inner = inner s' b.inner}
    | Func f -> let s' = (f.param, None)::s in 
      Closure {param = f.param; body = inner s' f.body}
    | Closure c -> let s' = (c.param, None)::s in
      Closure {c with body = inner s' c.body}
    | FuncApp fa -> FuncApp { f = inner s fa.f; param_act = inner s fa.param_act }
    | IfThenElse ite -> IfThenElse { cond = inner s ite.cond;
                                     then_branch = inner s ite.then_branch;
                                     else_branch = inner s ite.else_branch } in
  inner s e

(*
      print_endline "--------- f:";
      print_endline ([%derive.show: expression] f);
*)

let rec evaluate_exp s e =
  let eve_int s e = let e = evaluate_exp s e in
    match e with
      Immediate (Bool _) -> failwith "Type error; expected 'int' but got 'bool'"
    | _ -> e in
  let eve_bool s e = let e = evaluate_exp  s e in
    match e with
      Immediate (Integer _) -> failwith "Type error; expected 'bool' but got 'int'"
    | _ -> e in

  let evaluate_binop s b =
    let (ocaml_ev, op, lhs, rhs) = (match b with
          (Addition, lhs, rhs) -> (IntIntInt (+), Addition, eve_int s lhs, eve_int s rhs)
        | (Subtraction, lhs, rhs) ->
          (IntIntInt (-), Subtraction, eve_int s lhs, eve_int s rhs)
        | (Multiplication, lhs, rhs) ->
          (IntIntInt( * ), Multiplication, eve_int s lhs, eve_int s rhs)
        | (CompareEq, lhs, rhs) -> (IntIntBool(==), CompareEq, eve_int s lhs, eve_int s rhs)
        | (CompareLess, lhs, rhs) ->
          (IntIntBool(<), CompareLess, eve_int s lhs, eve_int s rhs)
        | (And, lhs, rhs) -> (BoolBoolBool(&&), And, eve_bool s lhs, eve_bool s rhs)
        | (Or, lhs, rhs) -> (BoolBoolBool(||), Or, eve_bool s lhs, eve_bool s rhs))
    in match (ocaml_ev, lhs, rhs) with
      (IntIntInt o_ev, Immediate (Integer lhs), Immediate (Integer rhs)) ->
      Immediate (Integer (o_ev lhs rhs))
    | (IntIntBool o_ev, Immediate (Integer lhs), Immediate (Integer rhs)) ->
      Immediate (Bool (o_ev lhs rhs))
    | (BoolBoolBool o_ev, Immediate (Bool lhs), Immediate (Bool rhs)) ->
      Immediate (Bool (o_ev lhs rhs))
    | (_, lhs, rhs) -> BinOperation (op, lhs, rhs)

  and evaluate_unop s b =
    let (ocaml_ev, op, operand) = (match b with
          (Negate, operand) -> (IntInt ((-) 0), Negate, eve_int s operand)
        | (Not, operand) -> (BoolBool (not), Not, eve_bool s operand))
    in match ocaml_ev, operand with
      IntInt ocaml_ev, Immediate (Integer operand) -> Immediate (Integer (ocaml_ev operand))
    |  BoolBool ocaml_ev, Immediate (Bool operand) -> Immediate (Bool (ocaml_ev operand))
    | _, operand -> UnOperation (op, operand) in

  let evaluate_var v f = let may_e = (try List.assoc v s with
        Not_found -> failwith (Printf.sprintf "Unbound variable '%s'" v.name)) in
    (match may_e with
       Some e -> f s e
     | None -> Variable v) in

  match e with
    Variable v -> evaluate_var v evaluate_exp
  | Immediate c -> e
  | BinOperation b -> evaluate_binop s b
  | UnOperation u -> evaluate_unop s u
  | Binding {is_rec; var; expr; inner} ->
    let s_rec = if is_rec then (var, None)::s else s in
    let expr' = evaluate_exp s_rec expr in
    let s' = (var, Some expr')::s in
    evaluate_exp s' inner
  | Func {param; body} ->
    let s' = (param, None)::s in
    let body' = substitute_from_state s' body in
    Closure { param; body = body' }
  | Closure c -> e
  | FuncApp {f; param_act} ->
    (match f with
       Closure {param; body} ->
       let body' = substitute param param_act body in
       evaluate_exp s body'
     | Variable v ->
       evaluate_var v (fun s e ->
           let app' = FuncApp { f = e; param_act = param_act } in
           evaluate_exp s app')
     | FuncApp fa ->
       let app' = FuncApp { f = evaluate_exp s f; param_act = param_act } in
       evaluate_exp  s app'
     | _ -> failwith "Type error: Function application requires function")

  | IfThenElse {cond; then_branch; else_branch} as ite ->
    let e = eve_bool s cond in
    (match e with
       Immediate (Bool true) -> evaluate_exp  s then_branch
     | Immediate (Bool false) -> evaluate_exp  s else_branch
     | _ -> ite )

let evaluate params p = let s = List.mapi (fun i p -> ({name = sprintf "@%d" i},
                                                       Some (Immediate (Integer p)))) params in
  match evaluate_exp s p with
    Immediate i -> i
  | _ -> failwith "Program does not evaluate to an integer or boolean value"

(*let () =
  Printf.printf "%d\n" (hyperfib 11 65);
  Printf.printf "%d\n" (coco 2147483645);*)