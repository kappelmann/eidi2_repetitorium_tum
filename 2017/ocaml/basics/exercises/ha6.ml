open Batteries

type var = int
type t = True | False | Var of var | Neg of t | Conj of t Set.t | Disj of t Set.t

let p s a b = if a <> "" then if b <> "" then a ^ s ^ b else a else b

let rec show = function
  | True -> "T"
  | False -> "F"
  | Var x -> "x" ^ string_of_int x
  | Neg e -> "Not(" ^ show e ^ ")"
  | Conj c -> "(" ^ Set.fold (fun x a -> p " /\\ " a (show x)) c "" ^ ")"
  | Disj d -> "(" ^ Set.fold (fun x a -> p " \\/ " a (show x)) d "" ^ ")"

let eval sigma = todo ()

let push_neg e = todo ()

let cleanup e = todo ()

let simplify e = todo ()

let is_simplified e = todo ()

let to_nnf e = todo ()

let to_dnf e = todo ()

let to_cnf e = todo ()
