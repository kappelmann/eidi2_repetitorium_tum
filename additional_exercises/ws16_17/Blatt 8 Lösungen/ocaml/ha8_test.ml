open Kaputt.Abbreviations
open Ha8_angabe
open Batteries.List 
open Ha8

module type Ha8 = sig
val get_in_edges : Ha8_angabe.cfg -> Ha8_angabe.node_id -> Ha8_angabe.edge list
val get_out_edges : Ha8_angabe.cfg -> Ha8_angabe.node_id -> Ha8_angabe.edge list
val must_compute_wp : Ha8_angabe.cfg -> Ha8_angabe.node_id -> bool
val can_compute_wp : Ha8_angabe.cfg -> Ha8_angabe.node_id -> bool
val set_wp : Ha8_angabe.cfg -> Ha8_angabe.node_id -> Ha8_angabe.formula -> Ha8_angabe.cfg
val normalize_boolean_expression : Ha8_angabe.boolean_expression -> Ha8_angabe.boolean_expression
val negate_boolean_expression : Ha8_angabe.boolean_expression -> Ha8_angabe.boolean_expression
val simplify_boolean_expression : Ha8_angabe.boolean_expression -> Ha8_angabe.boolean_expression
val simplify_implication : Ha8_angabe.implication -> Ha8_angabe.implication
val is_tautology : Ha8_angabe.implication -> bool
val simplify_formula : Ha8_angabe.formula -> Ha8_angabe.formula
val replace_var_in_linear_expression : Ha8_angabe.linear_expression -> Ha8_angabe.var -> Ha8_angabe.linear_expression -> Ha8_angabe.linear_expression
val replace_var_in_boolean_expression : Ha8_angabe.boolean_expression -> Ha8_angabe.var -> Ha8_angabe.linear_expression -> Ha8_angabe.boolean_expression
val compute_wp : Ha8_angabe.cfg -> Ha8_angabe.node_id -> Ha8_angabe.formula
val verify : Ha8_angabe.cfg -> Ha8_angabe.cfg
end

module X = Ha8
module Sol = Ha8_test_sol_impl

module Hint_printer = struct
  let next_cfg_id = ref 0

  let make_cfg_file_name i =
      "test_output_g" ^ (string_of_int i) ^ ".dot"

  let rec remove_files i =
      let fn = make_cfg_file_name i in
      if Sys.file_exists fn then (
          Sys.remove fn;
          remove_files (i+1))
      else ()

  let () = remove_files 0

  let print_edge_list (es : edge list) : string =
      let string_of_guard_opt = function
      | Some Yes -> "Some Yes"
      | Some No -> "Some No"
      | None -> "None"
      in
      let string_of_formula_opt = function
      | Some f -> "Some " ^ (string_of_formula f)
      | None -> "None"
      in
      "["^ String.concat "; " (List.map (fun (src, g, f, dst) -> 
          Printf.sprintf "(%d,%s,%s,%d)" src (string_of_guard_opt g) (string_of_formula_opt f) dst) es) ^ "]"

  let print_bool = string_of_bool

  let print_cfg (g : cfg) : string =
      let id = !next_cfg_id in
      next_cfg_id := (id + 1);
      let file_name = make_cfg_file_name id in
      Ha8_angabe.print_cfg g file_name; "<" ^ file_name ^ ">"

  let print_linear_expression = string_of_linear_expression
  let print_boolean_expression = string_of_boolean_expression ~dotfmt:false
  let print_implication = string_of_implication ~dotfmt:false
  let print_formula = string_of_formula ~dotfmt:false
  let print_option (f : ('a -> string)) (x : 'a option) : string =
      match x with None -> "None" | Some x' -> (f x')
end

module Nohint_printer = struct
  let print_edge_list = Assert.default_printer
  let print_bool = Assert.default_printer
  let print_cfg = Assert.default_printer
  let print_linear_expression = Assert.default_printer
  let print_boolean_expression = Assert.default_printer
  let print_implication = Assert.default_printer
  let print_formula = Assert.default_printer
  let print_option = (fun _ x -> Assert.default_printer x)
end

module P = Nohint_printer

(* additional example graphs *)
let cfg4 = {
  nodes = [
    (0, Start);
    (1, Statement (Read "m"));
    (2, Statement (Assignment ("a", Constant 0)));
    (3, Statement (Read "n"));
    (4, Branch (BoolExpr (LinExpr (1, "m", 0), Neq, LinExpr (1, "n", 0))));
    (5, Statement (Assignment ("a", LinExpr (42, "a", 0))));
    (6, Statement (Assignment ("a", Constant 42)));
    (7, Join);
    (8, Stop);
  ];
  edges = [
    (0, None, None, 1);
    (1, None, None, 2);
    (2, None, None, 3);
    (3, None, None, 4);
    (4, Some No, None, 5);
    (4, Some Yes, None, 6);
    (5, None, None, 7);
    (6, None, None, 7);
    (7, None, Some ([{ assumption=True; conclusion = BoolExpr (LinExpr (1, "a", 0), Eq, Constant 42)}]), 8);
  ]
}

let cfg5 = {
  nodes = [
    (0, Start);
    (1, Statement (Read "a"));
    (2, Statement (Read "b"));
    (3, Branch (BoolExpr (LinExpr (1, "a", 0), Le, LinExpr (1, "b", 0))));
    (4, Statement (Assignment ("t", LinExpr (1, "a", 0))));
    (5, Statement (Assignment ("a", LinExpr (1, "b", 0))));
    (6, Statement (Assignment ("b", LinExpr (1, "t", 0))));
    (7, Join);
    (8, Statement (Write "a"));
    (9, Stop);
  ];
  edges = [
    (0, None, None, 1);
    (1, None, None, 2);
    (2, None, None, 3);
    (3, Some No, None, 7);
    (3, Some Yes, None, 4);
    (4, None, None, 5);
    (5, None, None, 6);
    (6, None, None, 7);
    (7, None, None, 8);
    (8, None, Some ([{assumption = True; conclusion = BoolExpr (LinExpr (1, "b", 0), Leq, LinExpr (1, "a", 0)) }]), 9)
  ]
}

(* nicht verifizierbar! nur für spezielle tests *)
let cfg6 = {
  nodes = [
    (0, Start);
    (1, Statement (Read "x"));
    (2, Statement (Assignment ("y", Constant 4)));
    (3, Branch (BoolExpr (LinExpr (1, "x", 0), Eq, LinExpr (1, "y", 0))));
    (4, Statement (Assignment ("x", LinExpr (1, "x", 1))));
    (5, Statement (Assignment ("x", LinExpr (1, "x", (-1)))));
    (6, Join);
    (7, Statement (Write "x"));
    (8, Stop);
  ];
  edges = [
    (0, None, None, 1);
    (1, None, Some [{ assumption = True; conclusion = BoolExpr (LinExpr (1, "x", 0), Leq, Constant 0) }], 2);
    (2, None, None, 3);
    (3, Some No, None, 4);
    (3, Some Yes, Some [{ assumption = True; conclusion = BoolExpr (Constant 3, Neq, Constant 2) }], 5);
    (4, None, Some [{ assumption = True; conclusion = False }], 6);
    (5, None, None, 6);
    (6, None, None, 7);
    (7, None, Some [{ assumption = False; conclusion = True; }], 8);
  ]
}

let all_cfgs = [ cfg0; cfg1; cfg2; cfg3; cfg4; cfg5 ]

(* couple of helper functions *)
let get_nodes c = List.map (fun (id, _) -> id) c.nodes

let is_bexpr_equal a b = match (a, b) with
  | (BoolExpr (l1, Eq, r1), BoolExpr (l2, Eq, r2)) -> (l1 = l2 && r1 = r2) || (l1 = r2 && l2 = r1)
  | (BoolExpr (l1, Neq, r1), BoolExpr (l2, Neq, r2)) -> (l1 = l2 && r1 = r2) || (l1 = r2 && l2 = r1)
  | (l, r) -> l = r

let is_impl_equal { assumption = a1; conclusion = c1; } { assumption = a2; conclusion = c2 } =
  (is_bexpr_equal a1 a2) && (is_bexpr_equal c1 c2)

let is_formula_equal f1 f2 = 
  if List.length f1 <> List.length f2 then false else
    List.for_all2 (fun i1 i2 -> is_impl_equal i1 i2) f1 f2

let is_option_equal f x1 x2 = 
  match (x1, x2) with
  | (Some x1', Some x2') -> f x1' x2'
  | _ -> x1 = x2

type compute_tests = { graph : cfg; setup : (cfg -> cfg) list; n : node_id }

let update_node c nid n =
  { c with nodes = (nid, n) :: (List.remove_assoc nid c.nodes) }

let test_case ?eq:(compare=(=)) f printer input =
  Assert.equal ~msg:(printer input) ~eq:compare ~prn:printer
  (f (module Sol : Ha8) input) (f (module X : Ha8) input)


(* tests *)
(* 8.3 - 1 *)
let test_get_in_edges () = 
  let run_on_graph c = 
    List.iter (fun (id, _) -> Assert.equal ~prn:P.print_edge_list 
      (Sol.get_in_edges c id) (X.get_in_edges c id)) c.nodes in
  List.iter (run_on_graph) all_cfgs 

let test_get_out_edges () =
  let run_on_graph c =
    List.iter (fun (id, _) -> Assert.equal ~prn:P.print_edge_list 
      (Sol.get_out_edges c id) (X.get_out_edges c id)) c.nodes in
  List.iter (run_on_graph) all_cfgs

(* 8.3 - 2 *)
let test_must_compute_wp () = 
  let run_on_node c n =
    Assert.equal ~prn:P.print_bool (Sol.must_compute_wp c n) (X.must_compute_wp c n) 
  in
  List.iter (run_on_node cfg6) (get_nodes cfg6)

(* 8.3 - 3 *)
let test_can_compute_wp () =
  let run_on_node c n =
    Assert.equal ~prn:P.print_bool (X.can_compute_wp c n) (Sol.can_compute_wp c n)
  in
  List.iter (run_on_node cfg6) (get_nodes cfg6)

(* 8.3 - 4 *)
let test_set_wp () =
  let run_on_node c n =
    Assert.equal ~prn:P.print_cfg 
      (Sol.set_wp c n [{assumption = True; conclusion = BoolExpr(Constant 8, Neq, Constant 2) }])
      (X.set_wp c n [{assumption = True; conclusion = BoolExpr(Constant 8, Neq, Constant 2) }])      
  in 
  List.iter (run_on_node cfg1) (get_nodes cfg1)

(* 8.3 - 5 *)
let test_normalize_boolean_expression () =
  let cases = [
    True;
    False;
    BoolExpr (Constant 1, Eq, Constant 2);
    BoolExpr (Constant 1, Neq, Constant 2);
    BoolExpr (Constant 1, Leq, Constant 2);
    BoolExpr (Constant 1, Le, Constant 2);
    BoolExpr (LinExpr (6, "x", 23), Le, Constant (-5));
    BoolExpr (Constant 72, Le, LinExpr ((-4), "z", 0));
    BoolExpr (LinExpr (1, "k", 12), Le, LinExpr (13, "v", 8));
    BoolExpr (LinExpr (0, "f", 7), Eq, LinExpr (0, "u", (-9)));
    BoolExpr (LinExpr (0, "h", 2), Le, LinExpr (0, "b", 5))
  ]
  in 
  List.iter (test_case ~eq:is_bexpr_equal 
    (fun (module M : Ha8) -> M.normalize_boolean_expression) P.print_boolean_expression) cases

(* 8.3 - 6 *)
let test_negate_boolean_expression () =
  (* simple hack to relax contraints on negate_boolean_expression such that new solution
    works as well while the implementation is still backwards compatible *)
  let is_bexpr_equal_hack bexpr bexpr' =
    (is_bexpr_equal bexpr bexpr') || 
      (is_bexpr_equal (Sol.normalize_boolean_expression bexpr) bexpr')
  in
  let cases = [
    True;
    False;
    BoolExpr (Constant 1, Eq, LinExpr ((-3), "x", 7));
    BoolExpr (LinExpr (21, "b", 0), Neq, Constant 5);
    BoolExpr (LinExpr (7, "s", 11), Le, LinExpr (99, "n", 2));
    BoolExpr (LinExpr (81, "v", (-3)), Leq, LinExpr (18, "i", 6))
  ]
  in
  List.iter (test_case ~eq:is_bexpr_equal_hack (fun (module M : Ha8) -> M.negate_boolean_expression) 
    P.print_boolean_expression) cases


(* 8.3 - 7 *)
let test_simplify_boolean_expression () =
  let cases = [
    True;
    False;
    BoolExpr (Constant 18, Eq, LinExpr (0, "x", 12));
    BoolExpr (LinExpr (0, "y", 9), Neq, Constant 42);
    BoolExpr (Constant 17, Le, LinExpr (0, "u", (-4)));
    BoolExpr (LinExpr (0, "v", 11), Leq, Constant 11);
    BoolExpr (LinExpr (8, "k", 42), Eq, LinExpr (8, "k", 42));
    BoolExpr (LinExpr (7, "k", 42), Eq, LinExpr (8, "k", 42));

    BoolExpr (LinExpr (4, "x", 12), Eq, Constant 4);
    BoolExpr (LinExpr (4, "x", 12), Neq, Constant 4);
    BoolExpr (LinExpr (4, "x", 12), Le, Constant 4);
    BoolExpr (LinExpr (4, "x", 12), Leq, Constant 4);
    BoolExpr (LinExpr (4, "x", 12), Eq, LinExpr (20, "y", 20));
    BoolExpr (LinExpr (4, "x", 12), Neq, LinExpr (20, "y", 20));
    BoolExpr (LinExpr (4, "x", 12), Le, LinExpr (20, "y", 20));
    BoolExpr (LinExpr (4, "x", 12), Leq, LinExpr (20, "y", 20));
    BoolExpr (Constant 8, Eq, LinExpr (4, "x", 12));
    BoolExpr (Constant 8, Neq, LinExpr (4, "x", 12));
    BoolExpr (Constant 8, Le, LinExpr (4, "x", 12));
    BoolExpr (Constant 8, Leq, LinExpr (4, "x", 12));
  ]
  in
  List.iter (test_case (fun (module M : Ha8) -> M.simplify_boolean_expression) 
    P.print_boolean_expression) cases
  

(* 8.3 - 8 *)
let test_simplify_implication () =
  let cases = [
    { assumption = BoolExpr (LinExpr (4, "x", 12), Eq, Constant 4);
      conclusion = BoolExpr (LinExpr (4, "x", 12), Leq, LinExpr (20, "y", 20)); };
    { assumption = BoolExpr (LinExpr (0, "y", 9), Neq, Constant 42);
      conclusion = BoolExpr (Constant 8, Leq, LinExpr (4, "x", 12)); };
    { assumption = True; 
      conclusion = BoolExpr (LinExpr (4, "x", 12), Neq, LinExpr (20, "y", 20)); };
    { assumption = BoolExpr (LinExpr (0, "v", 11), Leq, Constant 11);
      conclusion = False; };
    { assumption = BoolExpr (LinExpr (4, "x", 12), Eq, LinExpr (20, "y", 20)); 
      conclusion = False; };
    { assumption = True; conclusion = False; }
  ]
  in
  List.iter (test_case ~eq:is_impl_equal (fun (module M : Ha8) -> M.simplify_implication) 
    P.print_implication) cases
    

(* 8.3 - 9 *)
let test_is_tautology () =
  let cases = [
    { assumption = False; 
      conclusion = BoolExpr (LinExpr (3, "x", 4), Eq, Constant 22) };
    { assumption = BoolExpr (Constant 18, Neq, Constant 18);
      conclusion = True };
    { assumption = BoolExpr (LinExpr (17, "z", (-4)), Neq, LinExpr ((-2), "k", 81));
      conclusion = BoolExpr (LinExpr (17, "z", (-4)), Neq, LinExpr ((-2), "k", 81)); };
    { assumption = BoolExpr (Constant 12, Eq, LinExpr (4, "b", 2));
      conclusion = BoolExpr (LinExpr (4, "b", 2), Eq, Constant 12) };
    { assumption = BoolExpr (LinExpr (3, "x", (-42)), Neq, LinExpr (1, "v", 12));
      conclusion = BoolExpr (LinExpr (1, "v", 12), Neq, LinExpr (3, "x", (-42))) };
    { assumption = BoolExpr (LinExpr (5, "z", (-3)), Leq, Constant 12);
      conclusion = BoolExpr (LinExpr (5, "z", (-3)), Leq, Constant 13) };
    { assumption = BoolExpr (LinExpr (3, "x", 0), Leq, LinExpr (6, "b", 22));
      conclusion = BoolExpr (LinExpr (3, "x", 0), Leq, LinExpr (6, "b", 28)) };
    { assumption = BoolExpr (Constant 4, Leq, LinExpr (8, "u", 21)); 
      conclusion = BoolExpr (Constant 3, Leq, LinExpr (8, "u", 21)) };
    { assumption = BoolExpr (LinExpr (12, "h", (-4)), Leq, LinExpr ((-7), "g", 2));
      conclusion = BoolExpr (LinExpr (12, "h", (-5)), Leq, LinExpr ((-7), "g", 2)) }
  ]
  in
  List.iter (fun impl -> Assert.equal ~prn:P.print_bool
    (Sol.is_tautology impl) (X.is_tautology impl)) cases

(* 8.3 - 10 *)
let test_simplify_formula () =
  let f = [
    { assumption = BoolExpr (Constant 4, Leq, LinExpr (3, "x", (-9)));
      conclusion = BoolExpr (LinExpr (15, "g", 0), Neq, LinExpr (3, "h", 18)) };
    { assumption = False; 
      conclusion = BoolExpr (LinExpr (3, "x", 4), Eq, Constant 22) };
    { assumption = BoolExpr (Constant 18, Neq, Constant 18);
      conclusion = True };
    { assumption = BoolExpr (LinExpr (17, "z", (-4)), Neq, LinExpr ((-2), "k", 81));
      conclusion = BoolExpr (LinExpr (17, "z", (-4)), Neq, LinExpr ((-2), "k", 81)); };
    { assumption = BoolExpr (Constant 12, Eq, LinExpr (4, "b", 2));
      conclusion = BoolExpr (LinExpr (4, "b", 2), Eq, Constant 12) };
    { assumption = BoolExpr (LinExpr (3, "x", (-42)), Neq, LinExpr (1, "v", 12));
      conclusion = BoolExpr (LinExpr (1, "v", 12), Neq, LinExpr (3, "x", (-42))) };
    { assumption = BoolExpr (LinExpr (5, "z", (-3)), Leq, Constant 12);
      conclusion = BoolExpr (LinExpr (5, "z", (-3)), Leq, Constant 13) };
    { assumption = BoolExpr (LinExpr (3, "x", 0), Leq, LinExpr (6, "b", 22));
      conclusion = BoolExpr (LinExpr (3, "x", 0), Leq, LinExpr (6, "b", 28)) };
    { assumption = BoolExpr (Constant 4, Leq, LinExpr (8, "u", 21)); 
      conclusion = BoolExpr (Constant 3, Leq, LinExpr (8, "u", 21)) };
    { assumption = BoolExpr (LinExpr (12, "h", (-4)), Leq, LinExpr ((-7), "g", 2));
      conclusion = BoolExpr (LinExpr (12, "h", (-5)), Leq, LinExpr ((-7), "g", 2)) };
    { assumption = True;
      conclusion = BoolExpr (LinExpr (39, "d", (-192)), Eq, Constant 4023) }
  ]
  in
  test_case ~eq:is_formula_equal (fun (module M : Ha8) -> M.simplify_formula) 
    P.print_formula f
    
(* 8.3 - 11 *)
let test_replace_var_in_linear_expression () =
  let cases = [
    Constant 8;
    Constant (-4);
    LinExpr (1, "a", 3);
    LinExpr (4, "a", (-39));
    LinExpr ((-12), "a", 0);
    LinExpr ((-42), "a", 42);
    LinExpr (3, "b", 0);
    LinExpr ((-4), "b", (-9));
  ]
  in
  List.iter (fun e -> List.iter (test_case (fun (module M : Ha8) ->
    M.replace_var_in_linear_expression e "a") P.print_linear_expression)
      cases) cases
  

let test_replace_var_in_boolean_expression () =
  let exprs = [
    Constant 8;
    Constant (-4);
    LinExpr (1, "a", 3);
    LinExpr (4, "a", (-39));
    LinExpr ((-12), "a", 0);
    LinExpr ((-42), "a", 42);
    LinExpr (3, "b", 0);
    LinExpr ((-4), "b", (-9));
  ]
  in let cases = [
      True;
      False;
      BoolExpr (LinExpr (18, "a", (-23)), Leq, LinExpr (4, "b", 9));
      BoolExpr (LinExpr (32, "a", 0), Neq, Constant 4);
      BoolExpr (Constant 19, Eq, LinExpr (21, "a", 99));
      BoolExpr (LinExpr (1, "b", 4), Leq, LinExpr (2, "a", 32));
      BoolExpr (LinExpr (18, "a", 2), Neq, LinExpr ((-4), "a", 1));
      BoolExpr (LinExpr (2, "b", (-1)), Eq, LinExpr (9, "d", 8));
    ]
  in
  List.iter (fun b -> 
    List.iter (fun e -> Assert.equal ~msg:(P.print_boolean_expression b) ~eq:is_bexpr_equal ~prn:P.print_boolean_expression
      (Sol.replace_var_in_boolean_expression b "a" e) (X.replace_var_in_boolean_expression b "a" e)) exprs) cases

(* 8.3 - 12 *)
let test_compute_wp () =
  let g0 = 
    { nodes = [
          (0, Start);
          (1, Statement (Read "x"));
          (2, Statement (Assignment ("x", Constant 0)));
          (3, Branch True);
          (4, Statement (Assignment ("x", LinExpr (1, "x", 0))));
          (5, Statement (Assignment ("x", LinExpr (1, "x", 0))));
          (6, Join);
          (7, Statement (Write "x"));
          (8, Stop);
        ];
      edges = [
        (0, None, None, 1);
        (1, None, None, 2);
        (2, None, None, 3);
        (3, Some No, None, 4);
        (3, Some Yes, None, 5);
        (4, None, None, 6);
        (5, None, None, 6);
        (6, None, None, 7);
        (7, None, None, 8);
      ]
    }
  in let f0 = 
       [{ assumption = BoolExpr (LinExpr (3, "a", (-3)), Leq, Constant 42);
          conclusion = BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0) }]
  in let f1 = 
       [{ assumption = True;
          conclusion = BoolExpr (LinExpr (2, "u", 3), Neq, LinExpr (18, "x", (-59))) }]
  in let f2 = 
       [{ assumption = False;
          conclusion = BoolExpr (Constant 2, Eq, Constant 3) }]
  in let f3 = 
       [{ assumption = BoolExpr (LinExpr (2, "x", 8), Leq, Constant 4);
          conclusion = BoolExpr (LinExpr (2, "x", 8), Leq, Constant 81) }]
  in
  let cases = [
    (* read *)
    { graph = g0; 
      setup = [
        (fun c -> Sol.set_wp c 2 f0);
        (fun c -> Sol.set_wp c 2 f1);
        (fun c -> Sol.set_wp c 2 f2);
        (fun c -> Sol.set_wp c 2 f3);
      ];
      n = 1; };
    (* write *)
    { graph = g0;
      setup = [
        (fun c -> Sol.set_wp c 8 f0);
        (fun c -> Sol.set_wp c 8 f1);
        (fun c -> Sol.set_wp c 8 f2);
        (fun c -> Sol.set_wp c 8 f3);
      ];
      n = 7; };
    (* assignment *)
    { graph = g0;
      setup = [
        (fun c -> Sol.set_wp c 3 [{ assumption = True; conclusion = BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0); }]);
        (fun c -> Sol.set_wp c 3 [{ assumption = True; conclusion = BoolExpr (LinExpr (0, "b", 0), Neq, LinExpr (1, "x", 0)); }]);
        (fun c -> update_node (Sol.set_wp c 3 [
            { assumption = BoolExpr (LinExpr (2, "x", (-15)), Le, Constant 38); 
              conclusion = BoolExpr (LinExpr (2, "b", 0), Eq, LinExpr (3, "b", 0)); }]) 
              2 (Statement (Assignment ("x", Constant 30))));
        (fun c -> update_node (Sol.set_wp c 3 [{ assumption = BoolExpr (LinExpr (12, "x", (-2)), Eq, LinExpr (15, "k", 8));
                                                 conclusion = False; }]) 2 (Statement (Assignment ("x", LinExpr ((-2), "k", 12)))));
        (fun c -> update_node (Sol.set_wp c 3 [{ assumption = BoolExpr (LinExpr (2, "v", 8), Le, LinExpr (1, "x", 0));
          conclusion = BoolExpr (LinExpr (2, "v", 3), Le, LinExpr (1, "x", 0)); }]) 2 (Statement (Assignment ("x", LinExpr (1, "l", 15)))))      ];
      n = 2;
    };
    (* join *)
    { graph = g0;
      setup = [
        (fun c -> Sol.set_wp c 7 f0);
        (fun c -> Sol.set_wp c 7 f1);
        (fun c -> Sol.set_wp c 7 f2);
        (fun c -> Sol.set_wp c 7 f3);
      ];
      n = 6; };
    (* branch *)
    { graph = g0;
      setup = [
        (fun c -> update_node (Sol.set_wp (Sol.set_wp c 4 [{ assumption = True; conclusion = BoolExpr (LinExpr (2, "x", 3), Neq, Constant 42); };
          { assumption = True; conclusion = BoolExpr (Constant 7, Leq, LinExpr ((-4), "g", 42)) }]) 5 
          [ { assumption = True; conclusion = BoolExpr (Constant 8, Neq, Constant 2); };
           { assumption = True; conclusion = BoolExpr (LinExpr ((-3), "g", 9), Leq, LinExpr (1, "g", -93)) }])
           3 (Branch (BoolExpr (LinExpr (1, "x", (-3)), Le, Constant 28))));
        (fun c -> update_node (Sol.set_wp (Sol.set_wp c 4 []) 5 [{
            assumption = True; conclusion = BoolExpr (LinExpr (13, "f", (-1)), Neq, LinExpr (31, "f", 120))
        }; { assumption = True; conclusion = BoolExpr (Constant 13, Le, LinExpr (3, "x", 0)) }])
          3 (Branch (BoolExpr (LinExpr (15, "z", 2), Eq, LinExpr (51, "k", (-4))))));
        (fun c -> update_node (Sol.set_wp (Sol.set_wp c 4 []) 5 [{ 
            assumption = True; conclusion = BoolExpr (LinExpr (1, "x", 8), Leq, Constant 31) }])
            3 (Branch (BoolExpr (LinExpr (1, "x", 10), Leq, Constant 31))));
      ];
      n = 3;
    }
  ]
  in
  List.iter (fun { graph; setup; n } -> 
    List.iter (fun f -> let g' = f graph in Assert.equal ~eq:is_formula_equal ~msg:(P.print_cfg graph)
      ~prn:P.print_formula (Sol.compute_wp g' n) (X.compute_wp g' n)) setup) cases 

(* 8.3 - 13 *)
let test_verify () =
  let get_program_condition cfg = 
    let (_, _, f, _) = List.find (fun (src, _, _, _) -> src = 0) cfg.edges in f
  in 
  let cases = [
    cfg0; cfg1; cfg2; cfg3; cfg4; cfg5
  ]
  in 
  List.iter (fun c -> Assert.equal ~eq:(is_option_equal is_formula_equal) ~prn:(P.print_option P.print_formula)
    ~msg:(P.print_cfg c) (get_program_condition (Sol.verify c)) (get_program_condition (X.verify c))) cases


(* /tests *)
let tests = ref []
let points = ref []
let add_points points_test test =
  points := !points @ [points_test];
  tests := !tests @ [test]

let () = 
  add_points  5 @@ Test.make_simple_test ~title:"get_in_edges" test_get_in_edges;
  add_points  5 @@ Test.make_simple_test ~title:"get_out_edges" test_get_out_edges;
  add_points  5 @@ Test.make_simple_test ~title:"must_compute_wp" test_must_compute_wp;
  add_points  5 @@ Test.make_simple_test ~title:"can_compute_wp" test_can_compute_wp;
  add_points 10 @@ Test.make_simple_test ~title:"set_wp" test_set_wp;
  add_points 10 @@ Test.make_simple_test ~title:"normalize_boolean_expression" test_normalize_boolean_expression;
  add_points 10 @@ Test.make_simple_test ~title:"negate_boolean_expression" test_negate_boolean_expression;
  add_points 30 @@ Test.make_simple_test ~title:"simplify_boolean_expression" test_simplify_boolean_expression;
  add_points 10 @@ Test.make_simple_test ~title:"simplify_implication" test_simplify_implication;
  add_points 20 @@ Test.make_simple_test ~title:"is_tautology" test_is_tautology;
  add_points 10 @@ Test.make_simple_test ~title:"simplify_formula" test_simplify_formula;
  add_points 10 @@ Test.make_simple_test ~title:"replace_var_in_linear_expression" test_replace_var_in_linear_expression;
  add_points 10 @@ Test.make_simple_test ~title:"replace_var_in_boolean_expression" test_replace_var_in_boolean_expression;
  add_points 40 @@ Test.make_simple_test ~title:"compute_wp" test_compute_wp;
  add_points 20 @@ Test.make_simple_test ~title:"verify" test_verify

(* launch *)
let () =
  Random.self_init ();
  let point = Test.(function
      | Passed -> 1
      | Report (p,n,e,c,m) when p = n -> 1
      | _ -> 0)
  in
  let passed = List.map point (Test.exec_tests !tests) in
  Test.run_tests ~output:(Test.Html_output (open_out "result.html")) !tests;
  Test.run_tests !tests;
  prerr_endline @@ "### GRADES: " ^ String.concat " "
  @@ List.map2 (fun x y -> string_of_int (y*x)) passed !points
