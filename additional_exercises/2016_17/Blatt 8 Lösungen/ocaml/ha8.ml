open Ha8_angabe

(* 8.3 - 1 *)
let get_in_edges (c : cfg) (n : node_id) : edge list = failwith "TODO"

let get_out_edges (c : cfg) (n : node_id) : edge list = failwith "TODO"

(* 8.3 - 2 *)
let must_compute_wp (c : cfg) (n : node_id) : bool = failwith "TODO"

(* 8.3 - 3 *)
let can_compute_wp (c : cfg) (n : node_id) : bool = failwith "TODO"

(* 8.3 - 4 *)
let set_wp (c : cfg) (n : node_id) (f : formula) : cfg = failwith "TODO"

(* 8.3 - 5 *)
let normalize_boolean_expression (bexpr : boolean_expression) : boolean_expression = failwith "TODO"

(* 8.3 - 6 *)
let negate_boolean_expression (bexpr : boolean_expression) : boolean_expression = failwith "TODO"

(* 8.3 - 7 *)
let rec simplify_boolean_expression (bexpr : boolean_expression) : boolean_expression = failwith "TODO"

(* 8.3 - 8 *)
let simplify_implication (impl : implication) : implication = failwith "TODO"

(* 8.3 - 9 *)
let is_tautology (impl : implication) : bool = failwith "TODO"

(* 8.3 - 10 *)
let simplify_formula (f : formula) : formula = failwith "TODO"

(* 8.3 - 11 *)
let replace_var_in_linear_expression (expr : linear_expression) (v : var) 
    (e : linear_expression) : linear_expression = failwith "TODO"

let replace_var_in_boolean_expression (bexpr : boolean_expression) (v : var) 
    (e : linear_expression) : boolean_expression = failwith "TODO"

(* 8.3 - 12 *)
let compute_wp (c : cfg) (n : node_id) : formula = failwith "TODO"

(* 8.3 - 13 *)
let rec verify (c : cfg) : cfg = failwith "TODO"
