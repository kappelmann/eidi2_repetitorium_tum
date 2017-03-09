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

let rec eval sigma = function
  | True -> true
  | False -> false
  | Var x -> sigma x
  | Neg e -> not (eval sigma e)
  | Conj c -> Set.for_all (fun x -> eval sigma x) c
  | Disj d -> Set.exists (fun x -> eval sigma x) d

let rec push_neg = function
  | Neg (Neg e) -> e
  | Neg True -> False
  | Neg False -> True
  | Neg (Conj c) -> Disj (Set.map (fun x -> Neg x) c) |> push_neg
  | Neg (Disj d) -> Conj (Set.map (fun x -> Neg x) d) |> push_neg
  | Conj c -> Conj (Set.map push_neg c)
  | Disj d -> Disj (Set.map push_neg d)
  | e -> e

let rec rule_neg = function
  | Neg (Neg e) -> e
  | Neg True -> False
  | Neg False -> True
  | Conj c -> Conj (Set.map rule_neg c)
  | Disj d -> Disj (Set.map rule_neg d)
  | e -> e

let rec rule_true_false = function
  | Conj c when Set.is_empty c -> True
  | Conj c when Set.mem False c -> False
  | Conj c when Set.exists (fun e1 -> Set.exists (fun e2 -> e1 = (Neg e2)) c) c -> False
  | Conj c -> Conj (Set.remove True c |> Set.map rule_true_false)
  | Disj d when Set.is_empty d -> False
  | Disj d when Set.mem True d -> True
  | Disj d when Set.exists (fun e1 -> Set.exists (fun e2 -> e1 = (Neg e2)) d) d -> True
  | Disj d -> Disj (Set.remove False d |> Set.map rule_true_false)
  | Neg e -> Neg (rule_true_false e)
  | e -> e

let rec rule_merge = function
  | Conj c1 -> ( match Set.to_list c1 |> List.find_map (function Conj c -> Some c | _ -> None) with
                 | None -> Conj (Set.map rule_merge c1)
                 | Some c2 -> let c1' = Set.remove (Conj c2) c1 in Conj (Set.union c1' c2)
               )
  | Disj d1 -> ( match Set.to_list d1 |> List.find_map (function Disj d -> Some d | _ -> None) with
                 | None -> Disj (Set.map rule_merge d1)
                 | Some d2 -> let d1' = Set.remove (Disj d2) d1 in Disj (Set.union d1' d2)
               )
  | Neg e -> Neg (rule_merge e)
  | e -> e

let rec rule_one = function
  (* | Conj c when Set.cardinal c = 1 -> Set.choose c *)
  | Conj c when Set.cardinal c = 1 -> Set.choose c |> Option.get_some
  | Conj c -> Conj (Set.map rule_one c)
  (* | Disj d when Set.cardinal d = 1 -> Set.choose d *)
  | Disj d when Set.cardinal d = 1 -> Set.choose d |> Option.get_some
  | Disj d -> Disj (Set.map rule_one d)
  | Neg e -> Neg (rule_one e)
  | e -> e

let rec rule_absorption = function
  | Conj c -> Set.find (function Disj d -> Set.exists (fun x -> Set.mem x d) c | _ -> false) c |>
              Option.map (fun x -> Conj (Set.remove x c)) |>
              Option.default (Conj (Set.map rule_absorption c))
  | Disj d -> Set.find (function Conj c -> Set.exists (fun x -> Set.mem x c) d | _ -> false) d |>
              Option.map (fun x -> Disj (Set.remove x d)) |>
              Option.default (Disj (Set.map rule_absorption d))
  | e -> e

let rec cleanup e =
  let e' = rule_one e |> rule_merge in
  if e <> e' then cleanup e' else e

let rec simplify e =
  let e' = rule_neg e |> rule_true_false |> rule_absorption in
  if e <> e' then simplify e' else e

let rec to_nnf e =
  let e' = push_neg e |> simplify |> cleanup in
  if e <> e' then to_nnf e' else e

let to_dnf e =
  let rec doit e =
    let e' =
      match e with
      | Conj c -> ( match Set.to_list c |> List.find_map (function Disj d -> Some d | _ -> None) with
                    | None -> Conj (Set.map doit c)
                    | Some d -> let c' = Set.remove (Disj d) c in Disj (Set.map (fun x -> Conj (Set.add x c')) d)
                  )
      | Disj d -> Disj (Set.map doit d)
      | e -> e
      in
    if e <> e' then doit e' else e in
  to_nnf e |> doit |> simplify |> cleanup

let to_cnf e =
  let rec doit e =
    let e' =
      match e with
      | Disj d -> ( match Set.to_list d |> List.find_map (function Conj c -> Some c | _ -> None) with
                    | None -> Disj (Set.map doit d)
                    | Some c -> let d' = Set.remove (Conj c) d in Conj (Set.map (fun x -> Disj (Set.add x d')) c)
                  )
      | Conj d -> Conj (Set.map doit d)
      | e -> e
      in
    if e <> e' then doit e' else e in
  to_nnf e |> doit |> simplify |> cleanup
