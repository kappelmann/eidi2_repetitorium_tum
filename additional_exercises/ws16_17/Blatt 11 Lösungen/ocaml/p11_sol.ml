(* 1. Aufgabe *)

type 'a expr =
    Const of 'a
  | Addition of 'a expr * 'a expr
  | Subtraction of 'a expr * 'a expr
  | Multiplication of 'a expr * 'a expr

module type ExprEvaluator = sig
  type elem
  type t = elem expr

  val evaluate : t -> elem
end

module type Ops = sig
  type elem

  val add : elem -> elem -> elem
  val sub : elem -> elem -> elem
  val mul : elem -> elem -> elem
end

module MakeExprEvaluator (O : Ops) :
  ExprEvaluator with type elem := O.elem = struct
  type elem = O.elem
  type t = elem expr

  let rec evaluate = function
      Const c -> c
    | Addition (a, b) -> O.add (evaluate a) (evaluate b)
    | Subtraction (a, b) -> O.sub (evaluate a) (evaluate b)
    | Multiplication (a, b) -> O.mul (evaluate a) (evaluate b)
end

module IntExprEvaluator = MakeExprEvaluator (struct
    type elem = int

    let add = (+)
    let sub = (-)
    let mul = ( * )
  end)

module FloatExprEvaluator = MakeExprEvaluator (struct
    type elem = float

    let add = (+.)
    let sub = (-.)
    let mul = ( *. )
  end)

module VectorExprEvaluator = MakeExprEvaluator (struct
    type elem = int list

    let op f a b = List.map2 f a b
    let add = op (+)
    let sub = op (-)
    let mul = op ( * )
  end)

let () =
  let exp = Subtraction ((Addition ((Const 3), (Const 4))), (Const 2)) in
  let v = IntExprEvaluator.evaluate exp in
  Printf.printf "%d\n" v;

(* 2. Aufgabe *)

(* Teil a *)
module type Number = sig
  type z
  val zero : z
  val ( +. ) : z -> z -> z
  val ( *. ) : z -> z -> z
  val string_of_number : z -> string
end

(* Teil a i *)
module Boolean = struct 
  type z = bool
  let zero = false
  let ( +. ) = ( || )
  let ( *. ) = ( && )
  let string_of_number = function true -> "1" | false -> "0"
end

(* Teil a ii *)
module MinPlusNat = struct
  type z = Value of int | Inf
  let zero = Inf
  let ( +. ) x y = 
    match (x,y) with
      Value x, Value y            -> Value(min x y)
    | Value x, Inf | Inf, Value x -> Value x
    | Inf,Inf                     -> Inf
  let ( *. ) x y = 
    match (x,y) with
      Value x, Value y -> Value(x + y)
    | _                -> Inf
  let string_of_number = function Value x -> string_of_int x | _ -> "inf"
end

(* Teil b *)
module type Matrix = sig
  type e
  type m
  val matrix_zero : m
  val ( **. ) : m -> m -> m
  val set_entry : m -> int * int -> e -> m
  val string_of_matrix : m -> string
end

exception NoImpl

(* Teil b i
   fuer b i reicht als erste Zeile
   module MakeMatrix (N : Number) = struct
*)
module MakeMatrix (N : Number) = struct
  open N
  type e = N.z 
  type m = (int * (int * z) list) list

  (* Vektor-Vektor-Multiplikation *)
  let rec v_mult p x y =
    match (x,y) with
      ((px,vx)::x',(py,vy)::y') -> 
      if px = py then
        v_mult (p +. vx *. vy) x' y'
      else if px < py then
        v_mult p x' y
      else
        v_mult p x y'
    | _ -> p

  let v_mult x y = v_mult zero x y

  (* Matrix * Vektor *)
  let m_v_mult a v =
    let l = List.map (fun (j,aj) -> (j, v_mult aj v)) a in  
    (* Multipliziert Zeilenvektor aj mit dem Vektor v *)
    List.filter (fun (j,aj) -> (aj <> zero)) l (* Nullen entfernen *)

  let rec v_to_list i = function
      (j,x)::v -> (i,j,x)::v_to_list i v
    | _ -> [] 

  let rec m_to_list = function
      (i,v)::a -> v_to_list i v @ m_to_list a
    | _ -> []

  let m_from_list l = 
    let l = List.sort compare l in
    List.fold_right 
      (
        fun (i,j,x) -> 
          function ((i',v)::vs) -> 
            if i = i' then 
              ((i,(j,x)::v)::vs) 
            else 
              (i,[(j,x)])::((i',v)::vs) 
                 | _ -> [(i,[(j,x)])]
      ) 
      l 
      [] 

  let transpose a =
    let l = m_to_list a in
    let l = List.map (fun (i,j,x) -> (j,i,x)) l in
    m_from_list l

  let ( **. ) a b = 
    let b = transpose b in
    let handle_row_a (i,ai) = (i, m_v_mult b ai) in 
    let r = List.map handle_row_a a in
    List.filter (fun (_,ai) -> ai <> []) r

  let rec vector_set_entry v i z =
    match v with
      []                      -> [(i,z)]
    | (i',z')::v' when i < i' -> (i,z)::v
    | (i',z')::v' when i = i' -> (i,z)::v'
    | (i',z')::v'             -> (i',z')::vector_set_entry v' i z

  let rec set_entry m (i,j) z = 
    match m with
      []                      -> [(i,[(j,z)])]
    | (i',v)::m' when i < i' -> (i,[(j,z)])::m
    | (i',v)::m' when i = i' -> (i,vector_set_entry v j z)::m'
    | (i',v)::m'             -> (i',v)::set_entry m' (i,j) z
  let matrix_zero = []
  let string_of_vector v =
    String.concat 
      ";" 
      (List.map (fun (i,z) -> "(" ^ string_of_int i ^ "," 
                              ^ string_of_number z ^ ")") v)
  let string_of_matrix m =
    String.concat 
      "\n" 
      (List.map (fun (i,v) -> string_of_int i ^ " -> " 
                              ^ string_of_vector v) m)
end

(* Teil b ii *)
module BooleanMatrix = MakeMatrix(Boolean)
open BooleanMatrix
let a = set_entry matrix_zero (1,1) true
let a = set_entry a    (2,2) true
let a = set_entry a    (3,3) true
let a = set_entry a    (1,2) true
let a = set_entry a    (2,3) true
let a = set_entry a    (3,1) true

module MinPlusNatMatrix = MakeMatrix(MinPlusNat)
open MinPlusNat
open MinPlusNatMatrix
let b = set_entry matrix_zero (1,1) (Value 0)
let b = set_entry b    (2,2) (Value 0)
let b = set_entry b    (3,3) (Value 0)
let b = set_entry b    (1,2) (Value 1)
let b = set_entry b    (2,3) (Value 1)
let b = set_entry b    (3,1) (Value 1)
let b = set_entry b    (1,3) (Value 5)
