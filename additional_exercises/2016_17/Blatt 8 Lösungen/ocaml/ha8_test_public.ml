open Kaputt.Abbreviations
open Ha8_angabe
open Batteries.List

module X = Ha8

module P = struct
    let next_graph_id = ref 0

    let make_graph_file_name i =
        "test_output_g" ^ (string_of_int i) ^ ".dot"

    let rec remove_graph_files i =
        let fn = make_graph_file_name i in
        if Sys.file_exists fn then  (
            Sys.remove fn;
            remove_graph_files (i+1)
        )
        else ()

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
        "[" ^ String.concat "; " (List.map (fun (src, g, f, dst) 
            -> Printf.sprintf "(%d,%s,%s,%d)" src 
                (string_of_guard_opt g) (string_of_formula_opt f) dst) es) ^ "]"

    let print_ocaml_formula (f : formula) : string =
        let print_ocaml_int i = if i < 0 then "("^(string_of_int i)^")" else (string_of_int i)
        in
        let print_ocaml_expr = function
            | Constant c -> "Constant "^(print_ocaml_int c)
            | LinExpr (a, x, b) -> "LinExpr ("^(print_ocaml_int a)^", \""^x^"\", "^(print_ocaml_int b)^")"
        in 
        let print_ocaml_bexpr = function
            | True -> "True"
            | False -> "False"
            | BoolExpr (l, op, r) -> "BoolExpr ("^(print_ocaml_expr l)^", "^
                (match op with Eq -> "Eq" | Neq -> "Neq" | Le -> "Le" | Leq -> "Leq")^
                ", "^(print_ocaml_expr r)^")"                    
        in
        let print_ocaml_impl impl =
            "{\n\tassumption=" ^ (print_ocaml_bexpr impl.assumption) ^
            ";\n\tconclusion=" ^ (print_ocaml_bexpr impl.conclusion) ^ "\n}\n"
        in
        "[" ^ (String.concat "; " (List.map print_ocaml_impl f)) ^ "]"

    let print_option (f : ('a -> string)) = function
        | None -> "None"
        | Some x -> "Some "^(f x) 

    let print_graph (g : cfg) : string =
        let id = !next_graph_id in
        next_graph_id := (id + 1);
        let file_name = make_graph_file_name id in
        print_cfg g file_name; "<" ^ file_name ^ ">"
end

let () = P.remove_graph_files 0

(* Zusätzlicher Graph fürs Testen *)
let test_cfg1 = {
    nodes = [
        (0, Start);
        (1, Statement (Read "i"));
        (2, Branch (BoolExpr (LinExpr (1, "i", 0), Neq, Constant 0)));
        (3, Statement (Assignment ("x", LinExpr (42, "i", 42))));
        (4, Statement (Assignment ("x", Constant (-42))));
        (5, Join);
        (6, Statement (Write "x"));
        (7, Stop);
    ];
    edges = [
        (0, None, None, 1);
        (1, None, None, 2);
        (2, Some No, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "i", 0), Eq, Constant 0)
        }], 3);
        (2, Some Yes, None, 4);
        (3, None, None, 5);
        (4, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "x", 0), Neq, Constant 0)
        }], 5);
        (5, None, Some [], 6);
        (6, None, Some [], 7)
    ]
}

let test_cfg2 = {
    nodes = test_cfg1.nodes;
    edges = [
        (0, None, None, 1);
        (1, None, Some [{
            assumption=True;
            conclusion=False }], 2);
        (2, Some No, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "i", 0), Eq, Constant 0)
        }], 3);
        (2, Some Yes, None, 4);
        (3, None, None, 5);
        (4, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "x", 0), Neq, Constant 0)
        }], 5);
        (5, None, Some [], 6);
        (6, None, Some [], 7)
    ]
}

let test_cfg3 = {
    nodes = test_cfg1.nodes;
    edges = [
        (0, None, None, 1);
        (1, None, None, 2);
        (2, Some No, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "i", 0), Eq, Constant 0)
        }], 3);
        (2, Some Yes, None, 4);
        (3, None, Some [{
            assumption=BoolExpr (Constant 42, Neq, LinExpr (1, "i", 0));
            conclusion=BoolExpr (LinExpr (1, "x", 0), Leq, Constant 0)
        }], 5);
        (4, None, Some [{
            assumption=BoolExpr (Constant 42, Neq, LinExpr (1, "i", 0));
            conclusion=BoolExpr (LinExpr (1, "x", 0), Leq, Constant 0)
        }], 5);
        (5, None, Some [], 6);
        (6, None, Some [], 7)
    ]
}

let test_cfg4 = {
    nodes = test_cfg1.nodes;
    edges = [
        (0, None, None, 1);
        (1, None, None, 2);
        (2, Some No, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "i", 0), Eq, Constant 0)
        }], 3);
        (2, Some Yes, Some [{
            assumption=True; conclusion=False}; {
            assumption=BoolExpr (Constant 1, Eq, Constant 2);
            conclusion=BoolExpr (LinExpr (2, "x", (-1)), Le, Constant 4)}], 4);
        (3, None, None, 5);
        (4, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "x", 0), Neq, Constant 0)
        }], 5);
        (5, None, Some [], 6);
        (6, None, Some [], 7)
    ]
}

let test_cfg5 = {
    nodes = [
        (0, Start);
        (1, Statement (Read "n"));
        (2, Branch (BoolExpr (LinExpr (2, "x", 0), Le, LinExpr (2, "n", 5))));
        (3, Statement (Assignment ("x", LinExpr (1, "y", (-3)))));
        (4, Statement (Assignment ("y", Constant 0)));
        (5, Join);
        (6, Statement (Write "x"));
        (7, Stop);
    ];
    edges = [
        (1, None, Some [
            {
                assumption=BoolExpr (LinExpr (1, "x", 0), Le, Constant 1);
                conclusion=BoolExpr (LinExpr (1, "n", 0), Eq, LinExpr (1, "y", 0))
            }
        ], 2);
        (2, Some No, Some [
            {
                assumption=True;
                conclusion=BoolExpr (LinExpr (1, "x", 0), Neq, Constant 0)
            }
        ], 3);
        (2, Some Yes, Some [
            {
                assumption=True;
                conclusion=BoolExpr (LinExpr (1, "x", 0), Leq, LinExpr (1, "n", 7))
            }; {
                assumption=True;
                conclusion=BoolExpr (LinExpr (1, "n", 0), Eq, Constant 19)
            }
        ], 4);
        (3, None, Some [
            {
                assumption=True;
                conclusion=BoolExpr (LinExpr (2, "x", 0), Eq, Constant 5)
            }; {
                assumption=True;
                conclusion=BoolExpr (LinExpr (1, "a", 0), Neq, LinExpr (1, "b", 0));
            }; {
                assumption=BoolExpr (LinExpr (1, "x", 0), Leq, LinExpr (5, "a", (-3)));
                conclusion=BoolExpr (LinExpr (1, "y", 0), Leq, LinExpr (5, "a", 2))
            }
        ], 5);
        (4, None, Some [
            {
                assumption=BoolExpr (LinExpr (1, "y", 0), Eq, Constant 1);
                conclusion=BoolExpr (LinExpr (1, "x", 0), Leq, Constant 3);
            }; {
                assumption=True;
                conclusion=BoolExpr (LinExpr (2, "x", 0), Leq, Constant 7) 
            }; {
                assumption=True;
                conclusion=BoolExpr (LinExpr (1, "y", 0), Eq, Constant 0);
            }; {
                assumption=BoolExpr (LinExpr (1, "a", 0), Neq, LinExpr (1, "b", 0));
                conclusion=BoolExpr (LinExpr (3, "y", 0), Eq, Constant 0)
            }
        ], 5);
        (5, None, Some [
            {
                assumption=BoolExpr (LinExpr (1, "x", 0), Leq, LinExpr (1, "n", 0));
                conclusion=BoolExpr (Constant 5, Leq, Constant 4)
            }
        ], 6);
        (6, None, Some [
            { 
                assumption=True;
                conclusion=BoolExpr (LinExpr (1, "y", 0), Neq, LinExpr (1, "x", 0))
            }; {
                assumption=BoolExpr (LinExpr (1, "n", (-4)), Leq, Constant 7);
                conclusion=BoolExpr (LinExpr (1, "y", 0), Eq, LinExpr ((-2), "x", 0))
            }
        ], 7);
    ]
}

let cfg0_result = {
    cfg0 with edges = [
        (0, None, Some [{
            assumption=True;
            conclusion=BoolExpr (Constant 1, Leq, LinExpr (2, "i", 0))
        }
        ], 1);
        (1, None, Some [{
            assumption=True;
            conclusion=BoolExpr (Constant 1, Leq, LinExpr (2, "i", 0))
        }
        ], 2);
        (2, None, Some [{
            assumption=True;
            conclusion=BoolExpr (Constant 0, Le, LinExpr (1, "f", 0))
        }
        ], 3);
    ]
}

let cfg1_result = {
    cfg1 with edges = [
        (0, None, Some [], 1);
        (1, None, Some [], 2);
        (2, None, Some [{
            assumption=BoolExpr (LinExpr (1, "c", 0), Leq, LinExpr (1, "i", (-1)));
            conclusion=BoolExpr (Constant (-1), Leq, LinExpr (1, "c", 0))
        }
        ], 3);
        (3, Some Yes, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "i", 0), Leq, LinExpr (1, "c", 0))
        }
        ], 5);
        (3, Some No, Some [{
            assumption=True;
            conclusion=BoolExpr (Constant (-1), Leq, LinExpr (1, "c", 0))
        }
        ], 4);
        (4, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "i", 0), Leq, LinExpr (1, "c", 1))
        }
        ], 6);
        (5, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "i", 0), Leq, LinExpr (1, "c", 1))
        }
        ], 6);
        (6, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "i", 0), Leq, LinExpr (1, "c", 1))
        }
        ], 7);
        (7, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "i", 0), Leq, LinExpr (1, "c", 1))
        }
        ], 8);
    ]
}

let cfg2_result = {
    cfg2 with edges = [
        (0, None, Some [], 1);
        (1, None, Some [{
            assumption=BoolExpr (LinExpr (1, "k", 0), Leq, Constant 130);
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }; {
            assumption=BoolExpr (Constant 131, Leq, LinExpr (1, "k", 0));
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }], 2);
        (2, None, Some [{
            assumption=BoolExpr (LinExpr (1, "k", 0), Leq, Constant 130);
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }; {
            assumption=BoolExpr (Constant 131, Leq, LinExpr (1, "k", 0));
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }], 3);
        (3, None, Some [{
            assumption=BoolExpr (LinExpr (1, "k", 0), Leq, Constant 130);
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }; {
            assumption=BoolExpr (LinExpr (1, "k", 0), Leq, Constant 130);
            conclusion=BoolExpr (LinExpr (21, "a", 0), Leq, LinExpr ((-1), "k", 2))
        }; {
            assumption=BoolExpr (Constant 131, Leq, LinExpr (1, "k", 0));
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }], 4);
        (4, None, Some [{
            assumption=BoolExpr (LinExpr (1, "k", 0), Leq, Constant 127);
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }; {
            assumption=BoolExpr (LinExpr (1, "k", 0), Leq, Constant 127);
            conclusion=BoolExpr (LinExpr (21, "a", 0), Leq, LinExpr ((-1), "k", (-1)))
        }; {
            assumption=BoolExpr (Constant 128, Leq, LinExpr (1, "k", 0));
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }], 5);
        (5, Some No, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (21, "a", 0), Leq, LinExpr ((-1), "k", (-1)))
        }], 6);
        (6, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (2, "x", 0), Eq, LinExpr (1, "b", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (21, "a", 0), Leq, LinExpr ((-1), "k", (-1)))
        }], 7);
        (7, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (2, "x", 0), Eq, LinExpr (1, "b", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (21, "a", 0), Leq, LinExpr (1, "k", (-1)))
        }], 8);
        (8, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (2, "x", 0), Eq, LinExpr (1, "b", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "a", 0), Leq, LinExpr (1, "k", (-1)))
        }], 9);
        (9, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (2, "x", 0), Eq, LinExpr (1, "b", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", (-1)))
        }], 10);
        (10, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (2, "x", 0), Eq, LinExpr (1, "b", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", (-1)))
        }], 23);
        (5, Some Yes, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }; {
            assumption=True;
            conclusion=BoolExpr (Constant 120, Leq, LinExpr (1, "k", 0))
        }], 11);
        (11, None, Some [{
            assumption=BoolExpr (LinExpr (1, "j", 0), Neq, Constant 17);
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }; {
            assumption=BoolExpr (LinExpr (1, "j", 0), Neq, Constant 17);
            conclusion=BoolExpr (Constant 120, Leq, LinExpr (1, "k", 0))
        }; {
            assumption=BoolExpr (LinExpr (1, "j", 0), Eq, Constant 17);
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", (-1)))
        }], 12);
        (12, None, Some [{
            assumption=BoolExpr (LinExpr (1, "j", 0), Neq, LinExpr (4, "a", 1));
            conclusion=BoolExpr (Constant 4, Eq, LinExpr (1, "a", 0))
        }; {
            assumption=BoolExpr (LinExpr (1, "j", 0), Neq, LinExpr (4, "a", 1));
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }; {
            assumption=BoolExpr (LinExpr (1, "j", 0), Neq, LinExpr (4, "a", 1));
            conclusion=BoolExpr (LinExpr (28, "a", 0), Leq, LinExpr (1, "k", (-8)))
        }; {
            assumption=BoolExpr (LinExpr (1, "j", 0), Eq, LinExpr (4, "a", 1));
            conclusion=BoolExpr (Constant 17, Eq, LinExpr (1, "j", 0))
        }; {
            assumption=BoolExpr (LinExpr (1, "j", 0), Eq, LinExpr (4, "a", 1));
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", (-1)))
        }], 13);
        (13, Some No, Some [{
            assumption=True;
            conclusion=BoolExpr (Constant 4, Eq, LinExpr (1, "a", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (28, "a", 0), Leq, LinExpr (1, "k", (-8)))
        }], 14);
        (14, None, Some [{
            assumption=True;
            conclusion=BoolExpr (Constant 16, Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", (-8)))
        }], 15);
        (15, None, Some [{
            assumption=True;
            conclusion=BoolExpr (Constant 16, Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", (-8)))
        }], 16);
        (16, None, Some [{
            assumption=True;
            conclusion=BoolExpr (Constant 17, Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", (-1)))
        }], 20);
        (13, Some Yes, Some [{
            assumption=True;
            conclusion=BoolExpr (Constant 17, Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", (-1)))
        }], 17);
        (17, None, Some [{
            assumption=True;
            conclusion=BoolExpr (Constant 17, Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", (-1)))
        }], 18);
        (18, None, Some [{
            assumption=True;
            conclusion=BoolExpr (Constant 17, Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", (-1)))
        }], 19);
        (19, None, Some [{
            assumption=True;
            conclusion=BoolExpr (Constant 17, Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", (-1)))
        }], 20);
        (20, None, Some [{
            assumption=True;
            conclusion=BoolExpr (Constant 17, Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", (-1)))
        }], 21);
        (21, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", (-1)))
        }], 22);
        (22, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (2, "x", 0), Eq, LinExpr (1, "b", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", (-1)))
        }], 23);
        (23, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (2, "x", 0), Eq, LinExpr (1, "b", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", (-1)))
        }], 24);
        (24, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (2, "x", 0), Eq, LinExpr (1, "b", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", 3))
        }], 25);
        (25, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (2, "x", 0), Eq, LinExpr (1, "b", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", 0), Leq, LinExpr (1, "k", 3))
        }], 26);
        (26, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, LinExpr (1, "j", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (2, "x", 0), Eq, LinExpr (1, "b", 0))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (7, "j", (-4)), Le, LinExpr (1, "k", 0))
        }], 27);
    ]
}

let cfg3_result = {
    cfg3 with edges = [
        (0, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, Constant 0)
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "b", 0), Eq, Constant 0)
        }], 1);
        (1, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, Constant (-3))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "b", 0), Eq, Constant 0)
        }], 2);
        (2, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, Constant (-3))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "b", 0), Eq, Constant 4)
        }], 3);
        (3, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, Constant (-6))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "b", 0), Eq, Constant 4)
        }], 4);
        (4, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, Constant (-6))
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "b", 0), Eq, Constant 13)
        }], 5);
        (5, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, Constant 0)
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "b", 0), Eq, Constant 13)
        }], 6);
        (6, None, Some [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, Constant 0)
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "b", 0), Eq, Constant 0)
        }], 7);
    ]
}

let add e o = match e with
        | Constant c -> Constant (c + o)
        | LinExpr (a, x, b) -> LinExpr (a, x, (b+o))

(* Test 8.3 - 1: get_in_edges *)
let test_get_in_edges () = 
    let test (input_graph, input_node_id, expected_output) =
        Assert.equal ~msg:("get_in_edges cfg1 "^(string_of_int input_node_id))
            ~prn:P.print_edge_list expected_output
            (X.get_in_edges input_graph input_node_id)
    in
    List.iter test [
        (cfg1, 0, []);
        (cfg1, 1, [(0, None, None, 1)]);
        (cfg1, 2, [(1, None, None, 2)]);
        (cfg1, 3, [(2, None, None, 3)]);
        (cfg1, 4, [(3, Some No, None, 4)]);
        (cfg1, 5, [(3, Some Yes, None, 5)]);
        (cfg1, 6, [(4, None, None, 6); (5, None, None, 6)]);
        (cfg1, 7, [(6, None, None, 7)]);
        (cfg1, 8, [(7, None, Some ([{
            assumption = True;
            conclusion = BoolExpr (LinExpr (1, "i", 0), Leq, LinExpr (1, "c", 1)) }]), 8)]);
    ]

(* Test 8.3 - 1: get_out_edges *)
let test_get_out_edges () =
    let test (input_graph, input_node_id, expected_output) =
        Assert.equal ~msg:("get_out_edges cfg1 "^(string_of_int input_node_id))
            ~prn:P.print_edge_list expected_output
            (X.get_out_edges input_graph input_node_id)
    in
    List.iter test [
        (cfg1, 0, [(0, None, None, 1)]);
        (cfg1, 1, [(1, None, None, 2)]);
        (cfg1, 2, [(2, None, None, 3)]);
        (cfg1, 3, [(3, Some Yes, None, 5); (3, Some No, None, 4)]);
        (cfg1, 4, [(4, None, None, 6)]);
        (cfg1, 5, [(5, None, None, 6)]);
        (cfg1, 6, [(6, None, None, 7)]);
        (cfg1, 7, [(7, None, Some ([{ 
            assumption = True; 
            conclusion = BoolExpr (LinExpr (1, "i", 0), Leq, LinExpr (1, "c", 1)) }]), 8)]);
        (cfg1, 8, [])
    ]

(* Test 8.3 - 2: must_compute_wp *)
let test_must_compute_wp () =
    let test (input_graph, input_node_id, expected_output) =
        Assert.equal ~msg:("must_compute_wp test_cfg1 "^(string_of_int input_node_id))
            ~prn:string_of_bool expected_output
            (X.must_compute_wp input_graph input_node_id)
    in
    List.iter test [
        (test_cfg1, 0, false);
        (test_cfg1, 1, true);
        (test_cfg1, 2, true);
        (test_cfg1, 3, false);
        (test_cfg1, 4, true);
        (test_cfg1, 5, true);
        (test_cfg1, 6, false);
        (test_cfg1, 7, false)
    ]

(* Test 8.3 - 3: can_compute_wp *)
let test_can_compute_wp () =
    let test (input_graph, input_node_id, expected_output) =
        Assert.equal ~msg:("can_compute_wp test_cfg1 "^(string_of_int input_node_id))
            ~prn:string_of_bool expected_output
            (X.can_compute_wp input_graph input_node_id)
    in
    List.iter test [
        (test_cfg1, 1, false);
        (test_cfg1, 2, false);
        (test_cfg1, 3, false);
        (test_cfg1, 4, true);
        (test_cfg1, 5, true);
        (test_cfg1, 6, true);
    ]

(* Test 8.3 - 4: set_wp *)
let test_set_wp () =
    let test (input_graph, input_node_id, input_formula, expected_output) =
        Assert.equal ~msg:("set_wp test_cfg1 "^(string_of_int input_node_id)^" "
            ^(string_of_formula input_formula)) ~prn:P.print_graph
            expected_output (X.set_wp input_graph input_node_id input_formula)
    in
    List.iter test [
        (test_cfg1, 2, [{ assumption=True; conclusion=False; }], test_cfg2);
        (test_cfg1, 5, [{ 
            assumption=BoolExpr (Constant 42, Neq, LinExpr (1, "i", 0));
            conclusion=BoolExpr (LinExpr (1, "x", 0), Leq, Constant 0)
        }], test_cfg3);
        (test_cfg1, 4, [{ 
            assumption=True; 
            conclusion=False
            }; {
            assumption=BoolExpr (Constant 1, Eq, Constant 2);
            conclusion=BoolExpr (LinExpr (2, "x", (-1)), Le, Constant 4)
        }], test_cfg4)
    ]

(* Test 8.3 - 5: normalize_boolean_expression *)
let test_normalize_boolean_expression () =
    let test (input_bexpr, expected_output) =
        Assert.equal ~msg:(string_of_boolean_expression input_bexpr)
            ~prn:string_of_boolean_expression
            expected_output (X.normalize_boolean_expression input_bexpr)
    in
    List.iter test ([
        (True, True);
        (False, False);
        (BoolExpr (Constant 7, Le, Constant 2), BoolExpr (Constant 7, Leq, Constant 1));
        (BoolExpr (Constant 11, Le, LinExpr (2, "x", 5)),
            BoolExpr (Constant 11, Leq, LinExpr (2, "x", 4)));
        (BoolExpr (LinExpr (3, "y", (-8)), Le, Constant (-81)), 
            (BoolExpr (LinExpr (3, "y", (-8)), Leq, Constant (-82))));
        (BoolExpr (LinExpr ((-2), "a", 7), Le, LinExpr (9, "b", 3)),
            BoolExpr (LinExpr ((-2), "a", 7), Leq, LinExpr (9, "b", 2)));
        (BoolExpr (LinExpr (0, "x", 28), Eq, Constant 3),
            BoolExpr (Constant 28, Eq, Constant 3));
        (BoolExpr (Constant 91, Neq, LinExpr (0, "y", (-19))),
            BoolExpr (Constant 91, Neq, Constant (-19)));
        (BoolExpr (LinExpr (0, "u", 0), Leq, LinExpr (0, "v", 1)),
            BoolExpr (Constant 0, Leq, Constant 1));
        (BoolExpr (LinExpr (0, "m", 22), Le, LinExpr (0, "n", 6)),
            BoolExpr (Constant 22, Leq, Constant 5))
    ] @ 
    List.concat (List.map (fun op ->
        [
        (let be = BoolExpr (Constant 7, op, Constant 2) in (be, be));
        (let be = BoolExpr (Constant 11, op, LinExpr (2, "x", 5)) in (be, be));
        (let be = BoolExpr (LinExpr (3, "y", (-8)), op, Constant (-81)) in (be, be));
        (let be = BoolExpr (LinExpr ((-2), "a", 7), op, LinExpr (9, "b", 3)) in (be, be))     
        ])
        [Eq; Neq; Leq]
    ))

(* Test 8.3 - 6: negate_boolean_expression *)
let test_negate_boolean_expression () =
    let test (input_bexpr, expected_output) =
        Assert.equal ~msg:(string_of_boolean_expression input_bexpr)
            ~prn:string_of_boolean_expression expected_output
            (X.negate_boolean_expression input_bexpr)
    in
    List.iter test ([
        (True, False);
        (False, True);
    ] @ List.concat (List.map (fun (l, r) -> 
        [
            (BoolExpr (l, Eq, r), BoolExpr (l, Neq, r));
            (BoolExpr (l, Neq, r), BoolExpr (l, Eq, r));
            (BoolExpr (l, Le, r), BoolExpr (r, Leq, l));
            (BoolExpr (l, Leq, r), BoolExpr (r, Leq, (add l (-1))))
        ]) 
        [
            (Constant 1, Constant (-4));
            (Constant (-8), LinExpr (13, "x", 13));
            (LinExpr ((-32), "b", 81), Constant 17);
            (LinExpr (5, "h", 7), LinExpr (71, "i", 0))
        ])
    )   

(* Test 8.3 - 7: simplify_boolean_expression *)
let test_simplify_boolean_expression () =
    let test (input_bexpr, expected_output) =
        Assert.equal ~msg:(string_of_boolean_expression input_bexpr)
            ~prn:string_of_boolean_expression expected_output
            (X.simplify_boolean_expression input_bexpr)
    in
    List.iter test ([
        (True, True);
        (False, False);
        (BoolExpr (Constant 0, Eq, Constant 1), False);
        (BoolExpr (Constant 0, Neq, Constant 1), True);
        (BoolExpr (Constant 0, Le, Constant 1), True);
        (BoolExpr (Constant 0, Leq, Constant 1), True);
        (BoolExpr (Constant 1, Eq, Constant 1), True);
        (BoolExpr (Constant 1, Neq, Constant 1), False);
        (BoolExpr (Constant 1, Le, Constant 1), False);
        (BoolExpr (Constant 1, Leq, Constant 1), True);
        (BoolExpr (Constant 1, Eq, Constant 0), False);
        (BoolExpr (Constant 1, Neq, Constant 0), True);
        (BoolExpr (Constant 1, Le, Constant 0), False);
        (BoolExpr (Constant 1, Leq, Constant 0), False);
        (BoolExpr (LinExpr (0, "d", 7), Eq, LinExpr (0, "f", 2)), False);
        (BoolExpr (LinExpr (0, "a", 2), Neq, LinExpr (0, "a", (-9))), True);
        (BoolExpr (LinExpr (0, "u", 1), Le, LinExpr (0, "d", 2)), True);
        (BoolExpr (LinExpr (0, "f", 9), Leq, LinExpr (0, "s", (-13))), False);
        (BoolExpr (LinExpr (1, "x", 0), Eq, LinExpr (2, "x", 0)), 
            BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0));
        (BoolExpr (LinExpr (17, "u", (-39)), Eq, LinExpr (29, "u", (-39))),
            BoolExpr (LinExpr (1, "u", 0), Eq, Constant 0));
        (BoolExpr (LinExpr (2, "x", 0), Eq, LinExpr (2, "x", 0)), True);
        (BoolExpr (LinExpr (81, "k", 33), Eq, LinExpr (81, "k", 33)), True);
    ] @ List.concat (List.map (fun op ->
        [
            (BoolExpr (LinExpr (2, "a", 2), op, Constant 8),
                BoolExpr (LinExpr (1, "a", 0), op, Constant 3));
            (BoolExpr (LinExpr (7, "b", 0), op, Constant (-21)),
                BoolExpr (LinExpr (1, "b", 0), op, Constant (-3)));
            (BoolExpr (LinExpr (3, "x", 0), op, Constant 8),
                BoolExpr (LinExpr (3, "x", 0), op, Constant 8));
            (BoolExpr (LinExpr (5, "v", 12), op, Constant 23),
                BoolExpr (LinExpr (5, "v", 0), op, Constant 11));
            (BoolExpr (LinExpr (2, "f", 8), op, LinExpr (6, "g", 22)),
                BoolExpr (LinExpr (1, "f", 0), op, LinExpr (3, "g", 7)));
            (BoolExpr (LinExpr (12, "z", (-12)), op, LinExpr ((-48), "a", 0)),
                BoolExpr (LinExpr (1, "z", 0), op, LinExpr ((-4), "a", 1)));
            (BoolExpr (LinExpr (1, "a", 3), op, LinExpr (77, "a", 77)),
                BoolExpr (LinExpr (1, "a", 0), op, LinExpr (77, "a", 74)));
            (BoolExpr (LinExpr (3, "u", 1), op, LinExpr (2, "v", 6)),
                BoolExpr (LinExpr (3, "u", 0), op, LinExpr (2, "v", 5)));
            (BoolExpr (LinExpr (2, "x", (-4)), op, LinExpr (7, "a", 4)),
                BoolExpr (LinExpr (2, "x", 0), op, LinExpr (7, "a", 8)));
            (BoolExpr (LinExpr ((-7), "m", 22), op, LinExpr (21, "n", 0)),
                BoolExpr (LinExpr ((-7), "m", 0), op, LinExpr (21, "n", (-22))));
            (BoolExpr (Constant 2, op, LinExpr (9, "d", 2)),
                BoolExpr (Constant 0, op, LinExpr (1, "d", 0)));
            (BoolExpr (Constant 3, op, LinExpr (3, "s", 0)),
                BoolExpr (Constant 1, op, LinExpr (1, "s", 0)));
            (BoolExpr (Constant 12, op, LinExpr (5, "h", (-3))),
                BoolExpr (Constant 3, op, LinExpr (1, "h", 0)));
            (BoolExpr (Constant 18, op, LinExpr (22, "u", 3)),
                BoolExpr (Constant 15, op, LinExpr (22, "u", 0)));
            (BoolExpr (Constant 33, op, LinExpr (17, "m", (-19))),
                BoolExpr (Constant 52, op, LinExpr (17, "m", 0)))
        ])
        [Eq; Neq; Leq]) @ 
        [
            (BoolExpr (LinExpr ((-1), "d", 0), Leq, Constant 7),
                BoolExpr (Constant (-7), Leq, LinExpr (1, "d", 0)));
            (BoolExpr (LinExpr ((-3), "x", 7), Leq, Constant 1),
                BoolExpr (Constant 2, Leq, LinExpr (1, "x", 0)));
            (BoolExpr (LinExpr ((-6), "v", 12), Leq, Constant 0),
                BoolExpr (Constant 2, Leq, LinExpr (1, "v", 0)));
            (BoolExpr (LinExpr ((-3), "a", 3), Leq, LinExpr (6, "a", 9)),
                BoolExpr (LinExpr ((-2), "a", (-2)), Leq, LinExpr (1, "a", 0)));
            (BoolExpr (LinExpr ((-7), "d", (-5)), Leq, LinExpr ((-21), "k", 9)),
                BoolExpr (LinExpr (3, "k", (-2)), Leq, LinExpr (1, "d", 0)));
            (BoolExpr (Constant 0, Leq, LinExpr ((-2), "x", 0)),
                BoolExpr (LinExpr (1, "x", 0), Leq, Constant 0));
            (BoolExpr (Constant 21, Leq, LinExpr ((-7), "v", (-7))),
                BoolExpr (LinExpr (1, "v", 0), Leq, Constant (-4)));
            (BoolExpr (Constant 3, Leq, LinExpr ((-4), "d", 7)),
                BoolExpr (LinExpr (1, "d", 0), Leq, Constant 1));     
            (BoolExpr (LinExpr (3, "a", 3), Le, Constant 17),
                BoolExpr (LinExpr (3, "a", 0), Leq, Constant 13));
            (BoolExpr (LinExpr (3, "b", 1), Le, Constant 17),
                BoolExpr (LinExpr (1, "b", 0), Leq, Constant 5));
            (BoolExpr (LinExpr ((-2), "x", 3), Le, Constant 18),
                BoolExpr (Constant (-7), Leq, LinExpr (1, "x", 0)));
            (BoolExpr (LinExpr ((-7), "y", 3), Le, Constant 0),
                BoolExpr (LinExpr ((-7), "y", 0), Leq, Constant (-4)));
            (BoolExpr (LinExpr ((-3), "a", (-1)), Le, LinExpr (3, "b", 3)),
                BoolExpr (LinExpr ((-1), "b", (-1)), Leq, LinExpr (1, "a", 0)));
            (BoolExpr (LinExpr ((-4), "a", (-1)), Le, LinExpr (3, "b", 3)),
                BoolExpr (LinExpr ((-4), "a", 0), Leq, LinExpr (3, "b", 3)));
            (BoolExpr (Constant 2, Le, LinExpr (7, "t", 1)),
                BoolExpr (Constant 2, Leq, LinExpr (7, "t", 0)));
            (BoolExpr (Constant 2, Le, LinExpr (7, "i", 3)),
                BoolExpr (Constant 0, Leq, LinExpr (1, "i", 0)));
            (BoolExpr (Constant 8, Le, LinExpr ((-3), "x", 0)),
                BoolExpr (LinExpr (1, "x", 0), Leq, Constant (-3)));
        ]
    )

(* Test 8.3 - 8: simplify_implication *)
let test_simplify_implication () = 
    let test (input_impl, expected_output) =
        Assert.equal ~msg:(string_of_implication input_impl)
            ~prn:string_of_implication expected_output
            (X.simplify_implication input_impl)
    in
    List.iter test [
        ({ 
            assumption=BoolExpr (Constant 2, Eq, Constant 3);
            conclusion=BoolExpr (LinExpr (2, "x", 3), Le, LinExpr (4, "k", 8)) 
        }, {
            assumption=False;
            conclusion=BoolExpr (LinExpr (1, "x", 0), Leq, LinExpr (2, "k", 2))
        }); ({
            assumption=BoolExpr (Constant 12, Le, LinExpr ((-5), "d", 3));
            conclusion=BoolExpr (Constant 17, Le, Constant 22)
        }, {
            assumption=BoolExpr (LinExpr (1, "d", 0), Leq, Constant (-2));
            conclusion=True;
        }); ({
            assumption=BoolExpr (LinExpr (3, "x", 7), Eq, LinExpr (8, "x", 7));
            conclusion=BoolExpr (Constant 0, Neq, Constant 0)
        }, {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "x", 0), Neq, Constant 0)
        }); ({
            assumption=True;
            conclusion=False;
        }, {
            assumption=True;
            conclusion=False;
        }); ({
            assumption=BoolExpr (Constant 7, Eq, LinExpr (5, "g", 3));
            conclusion=BoolExpr (LinExpr ((-13), "x", 7), Leq, LinExpr (26, "h", 7));
        }, {
            assumption=BoolExpr (Constant 4, Eq, LinExpr (5, "g", 0));
            conclusion=BoolExpr (LinExpr ((-2), "h", 0), Leq, LinExpr (1, "x", 0))
        }); ({
            assumption=BoolExpr (LinExpr (3, "f", (-2)), Neq, LinExpr (0, "u", 22));
            conclusion=BoolExpr (Constant 17, Eq, Constant (-17))
        }, {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "f", 0), Eq, Constant 8)
        })
    ]

(* Test 8.3 - 9: is_tautology *)
let test_is_tautology () =
    let test (input_impl, expected_output) =
        Assert.equal ~msg:(string_of_implication input_impl)
            ~prn:string_of_bool expected_output
            (X.is_tautology input_impl)
    in
    List.iter test ((List.concat (List.map (fun b ->
        [
            ({ assumption=False; conclusion=b }, true);
            ({ assumption=b; conclusion=True }, true);
            ({ assumption=b; conclusion=b }, true)
        ])
        [
            True;
            False;
            BoolExpr (Constant 3, Eq, Constant 9);
            BoolExpr (LinExpr (3, "s", (-3)), Le, Constant 23);
            BoolExpr (Constant 19, Neq, LinExpr (99, "o", 22));
            BoolExpr (LinExpr (8, "f", 1), Leq, LinExpr (13, "b", 0))
        ])) 
        @ (List.concat (List.map (fun (l, r) ->
        [
            ({ assumption=BoolExpr (l, Eq, r); conclusion=BoolExpr (r, Eq, l)}, true);
            ({ assumption=BoolExpr (l, Neq, r); conclusion=BoolExpr (r, Neq, l)}, true);
        ])
        [
            (Constant 0, Constant 0);
            (Constant 13, Constant (-13));
            (Constant 18, LinExpr (3, "x", 14));
            (Constant (-8), LinExpr ((-8), "d", (-1)));
            (LinExpr (21, "d", (-4)), Constant 12);
            (LinExpr ((-9), "u", 29), Constant (-3));
            (LinExpr (1, "d", 3), LinExpr (3, "h", 9));
            (LinExpr (91, "p", (-3)), LinExpr (17, "v", 0))
        ]))
        @ (List.concat (List.map (fun (l, r) -> 
        [
            ({
                assumption=BoolExpr (l, Leq, r);
                conclusion=BoolExpr (l, Leq, (add r (-17)))
            }, false);
            ({
                assumption=BoolExpr (l, Leq, r);
                conclusion=BoolExpr (l, Leq, (add r (-1)))
            }, false);
            ({
                assumption=BoolExpr (l, Leq, r);
                conclusion=BoolExpr (l, Leq, r)
            }, true);
            ({
                assumption=BoolExpr (l, Leq, r);
                conclusion=BoolExpr (l, Leq, (add r 1))
            }, true);
            ({
                assumption=BoolExpr (l, Leq, r);
                conclusion=BoolExpr (l, Leq, (add r 17))
            }, true);
            ({
                assumption=BoolExpr (r, Leq, l);
                conclusion=BoolExpr ((add r (-17)), Leq, l)
            }, true);
            ({
                assumption=BoolExpr (r, Leq, l);
                conclusion=BoolExpr ((add r (-1)), Leq, l)
            }, true);
            ({
                assumption=BoolExpr (r, Leq, l);
                conclusion=BoolExpr (r, Leq, l)
            }, true);
            ({
                assumption=BoolExpr (r, Leq, l);
                conclusion=BoolExpr ((add r 1), Leq, l)
            }, false);
            ({
                assumption=BoolExpr (r, Leq, l);
                conclusion=BoolExpr ((add r 17), Leq, l)
            }, false)
        ])
        [
            (LinExpr (3, "x", 5), Constant 22);
            (LinExpr ((-1), "g", 17), Constant (-9));
            (LinExpr (7, "v", 1), LinExpr (3, "b", 12));
            (LinExpr (1, "h", 99), LinExpr (7, "h", 0))
        ]))
    )

(* Test 8.3 - 10: simplify_formula *)
let test_simplify_formula () =
    let test (input_formula, expected_output) =
        Assert.equal ~msg:(string_of_formula input_formula)
            ~prn:string_of_formula expected_output
            (X.simplify_formula input_formula)
    in
    List.iter test [
        ([ 
            {
                assumption=BoolExpr (LinExpr (3, "g", 3), Neq, LinExpr (13, "u", 9));
                conclusion=BoolExpr (Constant 3, Eq, LinExpr (0, "d", 3))
            }; { 
                assumption=BoolExpr (LinExpr (0, "x", 7), Eq, LinExpr (22, "y", 3));
                conclusion=False
            }
        ], [
            {
                assumption=True;
                conclusion=BoolExpr (Constant 4, Neq, LinExpr (22, "y", 0))
            }
        ]); ([
            {
                assumption=False;
                conclusion=BoolExpr (LinExpr (3, "z", 13), Leq, Constant 39)
            }; {
                assumption=BoolExpr (LinExpr (3, "v", 2), Le, LinExpr (9, "z", 9));
                conclusion=BoolExpr (LinExpr (71, "h", 8), Leq, LinExpr (12, "u", 3))
            }; {
                assumption=BoolExpr (LinExpr (14, "e", 2), Leq, LinExpr (99, "i", 4));
                conclusion=BoolExpr (LinExpr (14, "e", 2), Le, LinExpr (99, "i", 11))
            }
        ], [ 
            {
                assumption=BoolExpr (LinExpr (1, "v", 0), Leq, LinExpr (3, "z", 2));
                conclusion=BoolExpr (LinExpr (71, "h", 0), Leq, LinExpr (12, "u", (-5)))
            }
        ]); ([
            {
                assumption=BoolExpr (LinExpr (7, "m", 2), Eq, LinExpr (3, "o", 55));
                conclusion=BoolExpr (LinExpr (7, "m", 2), Eq, LinExpr (3, "o", 55))
            }; {
                assumption=BoolExpr (LinExpr (2, "j", (-4)), Neq, Constant 0);
                conclusion=BoolExpr (Constant 0, Neq, LinExpr (2, "j", (-4)))
            }; {
                assumption=True;
                conclusion=BoolExpr (LinExpr (3, "x", 8), Eq, LinExpr ((-3), "x", 8))
            }
        ], [
            {
                assumption=True;
                conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0)
            }
        ]); ([
            {
                assumption=BoolExpr (Constant (-9), Le, LinExpr (6, "u", 0));
                conclusion=BoolExpr (Constant (-11), Leq, LinExpr (6, "u", (-1)))
            }
        ], [            
        ]); ([
            {
                assumption=BoolExpr (Constant 0, Leq, Constant 18);
                conclusion=BoolExpr (LinExpr (14, "f", 0), Leq, LinExpr (28, "i", (-14)))
            }; {
                assumption=BoolExpr (LinExpr (3, "p", 1), Eq, Constant 10);
                conclusion=BoolExpr (Constant 10, Eq, LinExpr (3, "p", 1))
            }
        ], [
            {
                assumption=True;
                conclusion=BoolExpr (LinExpr (1, "f", 0), Leq, LinExpr (2, "i", (-1)))
            }
        ])
    ]

(* Test 8.3 - 11: replace_var_in_linear_expression *)
let test_replace_var_in_linear_expression () =
    let test (input_expr, input_v, input_e, expected_output) =
        Assert.equal ~msg:("replace_var_in_linear_expression "^(string_of_linear_expression input_expr)
            ^" "^input_v^" "^(string_of_linear_expression input_e))
            ~prn:string_of_linear_expression expected_output
            (X.replace_var_in_linear_expression input_expr input_v input_e)
    in
    List.iter test [
        (Constant 8, "x", LinExpr (3, "u", 2), Constant 8);
        (LinExpr (2, "x", 0), "y", Constant 3, LinExpr (2, "x", 0));
        (LinExpr (1, "y", (-3)), "a", LinExpr (22, "u", 0), LinExpr (1, "y", (-3)));
        (LinExpr (1, "s", 0), "s", Constant 9, Constant 9);
        (LinExpr (1, "n", 0), "n", LinExpr (7, "v", (-3)), LinExpr (7, "v", (-3)));
        (LinExpr (3, "x", (-4)), "x", Constant 7, Constant 17);
        (LinExpr (4, "x", 2), "x", LinExpr (3, "d", 5), LinExpr (12, "d", 22));
    ]

(* Test 8.3 - 11: replace_var_in_boolean_expression *)
let test_replace_var_in_boolean_expression () =
    let test (input_bexpr, input_v, input_e, expected_output) =
        Assert.equal ~msg:("replace_var_in_boolean_expresson "^(string_of_boolean_expression input_bexpr)
            ^" "^input_v^" "^(string_of_linear_expression input_e))
            ~prn:string_of_boolean_expression expected_output
            (X.replace_var_in_boolean_expression input_bexpr input_v input_e)
    in
    List.iter test [
        (True, "x", LinExpr (3, "i", 2), True);
        (False, "y", Constant 7, False);
        (BoolExpr (Constant 3, Eq, LinExpr (2, "x", 0)), "u", Constant 4, 
            BoolExpr (Constant 3, Eq, LinExpr (2, "x", 0)));
        (BoolExpr (LinExpr (4, "v", 9), Neq, LinExpr (2, "s", 0)), "v", Constant 9,
            BoolExpr (Constant 45, Neq, LinExpr (2, "s", 0)));
        (BoolExpr (LinExpr (4, "v", 9), Le, LinExpr (2, "s", 0)), "s", Constant 7,
            BoolExpr (LinExpr (4, "v", 9), Le, Constant 14));
        (BoolExpr (LinExpr ((-3), "x", 3), Leq, LinExpr (7, "x", (-1))), "x", LinExpr (2, "b", 1),
            BoolExpr (LinExpr ((-6), "b", 0), Leq, LinExpr (14, "b", 6)));
        (BoolExpr (LinExpr (3, "a", 4), Eq, LinExpr (3, "b", 5)), "a", LinExpr (1, "b", 4),
            BoolExpr (LinExpr (3, "b", 16), Eq, LinExpr (3, "b", 5)))        
    ]

(* Test 8.3 - 12: compute_wp *)
let test_compute_wp () =
    let test (input_cfg, input_node_id, expected_output) =
        Assert.equal ~msg:("compute_wp test_cfg5 "^(string_of_int input_node_id))
            ~prn:string_of_formula expected_output
            (X.compute_wp input_cfg input_node_id)
    in
    List.iter test [
        (test_cfg5, 1, [{
            assumption=BoolExpr (LinExpr (1, "x", 0), Leq, Constant 0);
            conclusion=BoolExpr (LinExpr (1, "n", 0), Eq, LinExpr (1, "y", 0))
        }]);
        (test_cfg5, 2, [{
            assumption=BoolExpr (LinExpr (2, "n", 0), Leq, LinExpr (2, "x", (-5)));
            conclusion=BoolExpr (LinExpr (1, "x", 0), Neq, Constant 0)
        }; {
            assumption=BoolExpr (LinExpr (1, "x", 0), Leq, LinExpr (1, "n", 2));
            conclusion=BoolExpr (LinExpr (1, "n", 0), Eq, Constant 19)
        }]);
        (test_cfg5, 3, [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (2, "y", 0), Eq, Constant 11)
        }; {
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "a", 0), Neq, LinExpr (1, "b", 0))
        }]);
        (test_cfg5, 4, [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (2, "x", 0), Leq, Constant 7)
        }]);
        (test_cfg5, 5, [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "n", 0), Leq, LinExpr (1, "x", (-1)))
        }]);
        (test_cfg5, 6, [{
            assumption=True;
            conclusion=BoolExpr (LinExpr (1, "y", 0), Neq, LinExpr (1, "x", 0))
        }; {
            assumption=BoolExpr (LinExpr (1, "n", 0), Leq, Constant 11);
            conclusion=BoolExpr (LinExpr (1, "y", 0), Eq, LinExpr ((-2), "x", 0))
        }])
    ]

(* Test 8.3 - 13: verify *)
let test_verify () =
    let get_cfg_name cfg = List.assoc cfg [
        (cfg0, "cfg0"); (cfg1, "cfg1"); (cfg2, "cfg2"); (cfg3, "cfg3");
    ] 
    in
    let test (input_cfg, expected_output) = 
        Assert.equal ~msg:(get_cfg_name input_cfg) ~prn:P.print_graph
        expected_output (X.verify input_cfg)
    in
    List.iter test [
        (cfg0, cfg0_result);
        (cfg1, cfg1_result);
        (cfg2, cfg2_result);
        (cfg3, cfg3_result)
    ] 

(* testing engine *)
let tests = ref []
let points = ref []
let add_points points_test test =
    points := !points @ [points_test];
    tests := !tests @ [test]

let () =
    add_points 5 @@ Test.make_simple_test ~title:"get_in_edges" test_get_in_edges;
    add_points 5 @@ Test.make_simple_test ~title:"get_out_edges" test_get_out_edges;
    add_points 5 @@ Test.make_simple_test ~title:"must_compute_wp" test_must_compute_wp;
    add_points 5 @@ Test.make_simple_test ~title:"can_compute_wp" test_can_compute_wp;
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