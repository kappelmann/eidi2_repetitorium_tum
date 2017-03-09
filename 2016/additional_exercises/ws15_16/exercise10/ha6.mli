type var = int
type t =
    True
  | False
  | Var of var
  | Neg of t
  | Conj of t Batteries.Set.t
  | Disj of t Batteries.Set.t
val show : t -> string
val eval : (var -> bool) -> t -> bool
val push_neg : t -> t
val cleanup : t -> t
val simplify : t -> t
val is_simplified : t -> bool
val to_nnf : t -> t
val to_dnf : t -> t
val to_cnf : t -> t
