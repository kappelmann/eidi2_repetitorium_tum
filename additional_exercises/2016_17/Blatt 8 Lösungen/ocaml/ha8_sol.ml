open Ha8_angabe

(* 8.3 - 1 *)
let get_in_edges (c : cfg) (n : node_id) : edge list = 
    List.filter (fun (_,_,_,dst) -> dst = n) c.edges

let get_out_edges (c : cfg) (n : node_id) : edge list = 
    List.filter (fun (src,_,_,_) -> src = n) c.edges

(* 8.3 - 2 *)
let must_compute_wp (c : cfg) (n : node_id) : bool = 
    (List.filter (fun (_, _, ass, _) -> ass = None) (get_in_edges c n)) <> []

(* 8.3 - 3 *)
let can_compute_wp (c : cfg) (n : node_id) : bool = 
    (List.filter (fun (_, _, ass, _) -> ass = None) (get_out_edges c n)) = []

(* 8.3 - 4 *)
let set_wp (c : cfg) (n : node_id) (f : formula) : cfg = 
    let new_edges = List.map (fun ((src, g, _, dst) as e) -> if dst = n then (src, g, Some f, dst) else e) c.edges in
    { c with edges = new_edges }

(* 8.3 - 5 *)
let normalize_boolean_expression (bexpr : boolean_expression) : boolean_expression = 
    let to_constant = function
    | LinExpr (0, _, b) -> Constant b
    | e -> e
    in 
    match bexpr with
    | True -> True
    | False -> False
    | BoolExpr (l, op, r) -> (
        let bexpr' = BoolExpr (to_constant l, op, to_constant r) in
        match bexpr' with
        | BoolExpr (l, Le, Constant c1) -> BoolExpr (l, Leq, Constant (c1-1))
        | BoolExpr (l, Le, LinExpr (a, x, b)) -> BoolExpr (l, Leq, LinExpr (a, x, (b-1)))
        | bexpr'' -> bexpr'')

(* 8.3 - 6 *)
let negate_boolean_expression (bexpr : boolean_expression) : boolean_expression =
    normalize_boolean_expression ( 
        match bexpr with
        | True -> False
        | False -> True
        | BoolExpr (l, Eq, r) -> BoolExpr (l, Neq, r)
        | BoolExpr (l, Neq, r) -> BoolExpr (l, Eq, r)
        | BoolExpr (l, Le, r) -> BoolExpr (r, Leq, l)
        | BoolExpr (l, Leq, r) -> BoolExpr (r, Le, l)
    )

(* 8.3 - 7 *)
let rec simplify_boolean_expression (bexpr : boolean_expression) : boolean_expression =
    match normalize_boolean_expression bexpr with
    | BoolExpr (Constant c0, Eq, Constant c1) -> if c0 = c1 then True else False
    | BoolExpr (Constant c0, Neq, Constant c1) -> if c0 <> c1 then True else False
    | BoolExpr (_, Le, _) -> failwith "Should be normalized here!"
    | BoolExpr (Constant c0, Leq, Constant c1) -> if c0 <= c1 then True else False
    | BoolExpr (LinExpr (a0, x0, b0), Eq, LinExpr (a1, x1, b1)) when x0 = x1 && b0 = b1
        -> if a0 = a1 then True else BoolExpr (LinExpr (1, x0, 0), Eq, Constant 0)
    | BoolExpr (LinExpr (a0, x0, b0), op, LinExpr (a1, x1, b1)) ->
        if a1 mod a0 = 0 && (b1-b0) mod a0 = 0 then 
            (if a0 < 0 && op = Leq then BoolExpr (LinExpr (a1/a0, x1, (b1-b0)/a0), Leq, LinExpr (1, x0, 0))
            else BoolExpr (LinExpr (1, x0, 0), op, LinExpr (a1/a0, x1, (b1-b0)/a0)))
        else BoolExpr (LinExpr (a0, x0, 0), op, LinExpr (a1, x1, (b1 - b0)))
    | BoolExpr (LinExpr (a, x, b), op, Constant c) ->
        if (c-b) mod a = 0 then 
            (if a < 0 && op = Leq then BoolExpr (Constant ((c-b)/a), Leq, LinExpr (1, x, 0))
            else BoolExpr (LinExpr (1, x, 0), op, Constant ((c-b)/a)))
        else BoolExpr (LinExpr (a, x, 0), op, Constant (c-b))
    | BoolExpr (Constant c, op, LinExpr (a, x, b)) ->
        if (c-b) mod a = 0 then
            (if a < 0 && op = Leq then BoolExpr (LinExpr (1, x, 0), Leq, Constant ((c-b)/a)) 
            else BoolExpr (Constant ((c-b)/a), op, LinExpr (1, x, 0)))
        else BoolExpr (Constant (c-b), op, LinExpr (a, x, 0))
    | bexpr' -> bexpr' 

(* 8.3 - 8 *)
let simplify_implication (impl : implication) : implication = 
    match { assumption = simplify_boolean_expression impl.assumption;
        conclusion = simplify_boolean_expression impl.conclusion } with
    | { assumption; conclusion = False } -> { assumption = True; conclusion = negate_boolean_expression assumption }
    | impl' -> impl'

(* 8.3 - 9 *)
let is_tautology (impl : implication) : bool = 
    let is_leq a b = match (a, b) with
    | (Constant c1, Constant c2) -> c1 <= c2
    | (LinExpr (a1, x1, b1), LinExpr (a2, x2, b2)) 
        when a1 = a2 && x1 = x2 -> b1 <= b2
    | _ -> false
    in
    match impl with
    | { assumption = False; } -> true
    | { conclusion = True; } -> true
    | { assumption; conclusion } when assumption = conclusion -> true
    | { assumption = BoolExpr (la, Eq, ra); conclusion = BoolExpr (lc, Eq, rc) } 
        when la = rc && ra = lc -> true
    | { assumption = BoolExpr (la, Neq, ra); conclusion = BoolExpr (lc, Neq, rc) }
        when la = rc && ra = lc -> true
    | { assumption = BoolExpr (la, Leq, ra); conclusion = BoolExpr (lc, Leq, rc) }
        when la = lc -> is_leq ra rc
    | { assumption = BoolExpr (la, Leq, ra); conclusion = BoolExpr (lc, Leq, rc) }
        when ra = rc -> is_leq lc la 
    | _ -> false

(* 8.3 - 10 *)
let simplify_formula (f : formula) : formula =
    let f' = List.map (fun i -> simplify_implication i) f in
    List.filter (fun i -> not (is_tautology i)) f'

(* 8.3 - 11 *)
let replace_var_in_linear_expression (expr : linear_expression) (v : var) 
    (e : linear_expression) : linear_expression = 
    match expr with
    | LinExpr (a, x, b) when v = x ->
        (match e with 
        | Constant c -> Constant ((a * c) + b)
        | LinExpr (a', x', b') -> LinExpr (a * a', x', (a * b') + b)
        )
    | expr' -> expr'

let replace_var_in_boolean_expression (bexpr : boolean_expression) (v : var) 
    (e : linear_expression) : boolean_expression = 
    match bexpr with
    | BoolExpr (l, op, r) -> BoolExpr (
        replace_var_in_linear_expression l v e, op, replace_var_in_linear_expression r v e)
    | bexpr' -> bexpr'   

let get_br_edges c n = 
    let es = get_out_edges c n in 
    let (_, _, Some no_pc, _) = List.find (fun (_, g, _, _) -> g = Some No) es in
    let (_, _, Some yes_pc, _) = List.find (fun (_, g, _, _) -> g = Some Yes) es in
    (no_pc, yes_pc)

(* 8.3 - 12 *)
let compute_wp (c : cfg) (n : node_id) : formula = 
    let posts = List.map (fun (_, _, Some f, _) -> f) (get_out_edges c n) in
    let wp = match List.assoc n c.nodes with
    | Statement (Read x) -> List.hd posts
    | Statement (Write x) -> List.hd posts
    | Statement (Assignment (x, e)) -> 
        List.map (fun { assumption; conclusion } -> 
            { assumption = replace_var_in_boolean_expression assumption x e;
                conclusion = replace_var_in_boolean_expression conclusion x e }) (List.hd posts)
    | Join -> List.hd posts 
    | Branch b -> let (no_pc, yes_pc) = get_br_edges c n  in
        (List.map (fun { assumption; conclusion } -> if assumption <> True then failwith "assumption must be true" 
            else { assumption = negate_boolean_expression b; conclusion = conclusion }) no_pc) @ 
        (List.map (fun { assumption; conclusion } -> if assumption <> True then failwith "assumption must be true"
            else { assumption = b; conclusion = conclusion }) yes_pc)
    | _ -> []
    in simplify_formula wp

(* 8.3 - 13 *)
let rec verify (c : cfg) : cfg = 
    let missing_wps = List.filter (fun (n,_) -> must_compute_wp c n) c.nodes in
    if missing_wps = [] then c else
    let computable_wps = List.filter (fun (n,_) -> can_compute_wp c n) missing_wps in
    if computable_wps = [] then c else
    let (id, _) = List.hd computable_wps in
    let wp = compute_wp c id in
    verify (set_wp c id wp)
