 
(* types provided *)
type var = string
type linear_expression = LinExpr of int * var * int | Constant of int
type boolean_expression_op = Eq | Neq | Le | Leq 
type boolean_expression = True | False | BoolExpr of linear_expression * boolean_expression_op * linear_expression
type implication = { assumption : boolean_expression; conclusion : boolean_expression } 
type formula = implication list

type instruction = Assignment of var * linear_expression | Read of var | Write of var
type node = Statement of instruction | Branch of boolean_expression | Join | Start | Stop

type node_id = int
type guard = No | Yes
type edge = node_id * guard option * formula option * node_id 
type cfg = { nodes : (node_id * node) list; edges : edge list }


(* string and output functions provided *)
let string_of_linear_expression ?dotfmt:(df=false) expr = if df then (match expr with
    | LinExpr (0, x, b) -> string_of_int b
    | LinExpr (1, x, 0) -> x
    | LinExpr (1, x, b) when b > 0 -> x ^ " + " ^ (string_of_int b)
    | LinExpr (1, x, b) when b < 0 -> x ^ " - " ^ (string_of_int (-b))
    | LinExpr (-1, x, 0) -> "-" ^ x
    | LinExpr (-1, x, b) when b > 0 -> "-" ^ x ^ " + " ^ (string_of_int b)
    | LinExpr (-1, x, b) when b < 0 -> "-" ^ x ^ " - " ^ (string_of_int (-b))
    | LinExpr (a, x, 0) -> (string_of_int a) ^ x
    | LinExpr (a, x, b) when b > 0 -> (string_of_int a) ^ x ^ " + " ^ (string_of_int b)
    | LinExpr (a, x, b) when b < 0 -> (string_of_int a) ^ x ^ " - " ^ (string_of_int (-b))
    | Constant c -> string_of_int c
) else (match expr with
    | Constant c -> string_of_int c
    | LinExpr (a, x, b) -> (string_of_int a)^x^"+"^(string_of_int b)
)


let string_of_boolean_expression ?dotfmt:(df=false) (cmp : boolean_expression) =
    let string_of_op = function
        | Eq -> " = " | Neq -> if df then " &ne; " else " != " 
        | Le -> " < " | Leq -> if df then " &le; " else " <= " 
    in
    match cmp with
    | True -> "true"
    | False -> "false"
    | BoolExpr (l, op, r) -> (string_of_linear_expression ~dotfmt:df l) ^ (string_of_op op) ^ 
        (string_of_linear_expression ~dotfmt:df r)
    

let string_of_implication ?dotfmt:(df=false) (impl : implication) =
    if impl.assumption = True then (string_of_boolean_expression ~dotfmt:df impl.conclusion) 
    else "(" ^ (string_of_boolean_expression ~dotfmt:df impl.assumption) ^ (if df then " &rArr; " else " ==> ") 
        ^ (string_of_boolean_expression ~dotfmt:df impl.conclusion) ^ ")"    

let string_of_formula ?dotfmt:(df=false) = function
    | [] -> "true"
    | impl :: tail -> List.fold_left (fun a i -> a ^ (if df then " &and; " else " && ") 
        ^ (string_of_implication ~dotfmt:df i)) (string_of_implication ~dotfmt:df impl) tail 

let string_of_instruction ?dotfmt:(df=false) = function
    | Assignment (v, e) -> v ^ " = " ^ (string_of_linear_expression ~dotfmt:df e)
    | Read v -> v ^ " = read()"
    | Write v -> "write(" ^ v ^ ")"

let print_cfg (g : cfg) (fname : string) : unit =
    let out = open_out fname in
    let print_cfg_node = function
        | (id, Start) -> Printf.fprintf out "\tn%i[shape=circle,label=\"Start\"];\n" id
        | (id, Stop) -> Printf.fprintf out "\tn%i[shape=circle,label=\"Stop\"];\n" id 
        | (id, Statement instr) -> Printf.fprintf out "\tn%i[shape=box,label=\"%s\"];\n" id (string_of_instruction ~dotfmt:true instr)
        | (id, Branch cmp) -> Printf.fprintf out "\tn%i[shape=diamond,label=\"%s\"];\n" id (string_of_boolean_expression ~dotfmt:true cmp)
        | (id, Join) -> Printf.fprintf out "\tn%i[shape=point];\n" id
    in  
    let print_cfg_edge (src, grd, ass, dst) =
        Printf.fprintf out "\tn%i -> n%i[taillabel=\"%s\",label=\"%s\"];\n" src dst
            (match grd with None -> "" | Some No -> "no" | Some Yes -> "yes") 
            (match ass with None -> "" | Some a -> string_of_formula ~dotfmt:true a)
    in
    begin
        Printf.fprintf out "digraph CFG {\n";
        List.iter print_cfg_node g.nodes;
        List.iter print_cfg_edge g.edges;
        Printf.fprintf out "}\n";
        close_out out
    end


(* example graphs provided *)
let cfg0 = {
    nodes = [
        (0, Start);
        (1, Statement (Read "i"));
        (2, Statement (Assignment ("f", LinExpr (2, "i", 0))));
        (3, Stop);
    ];
    edges = [
        (0, None, None, 1);
        (1, None, None, 2);
        (2, None, Some ([{ 
            assumption=True; 
            conclusion=BoolExpr (Constant 0, Le, LinExpr (1, "f", 0)) }]), 3)
    ]
}

let cfg1 = { 
    nodes = [
        (0, Start);
        (1, Statement (Read "i"));
        (2, Statement (Assignment ("c", Constant 20)));
        (3, Branch (BoolExpr (LinExpr (1, "i", 0), Leq, LinExpr (1, "c", 0))));
        (4, Statement (Assignment ("i", Constant 0)));
        (5, Statement (Assignment ("i", LinExpr (1, "i", 1))));
        (6, Join);
        (7, Statement (Write "i"));
        (8, Stop);
    ];
    edges = [
        (0, None, None, 1);
        (1, None, None, 2);
        (2, None, None, 3);
        (3, Some Yes, None, 5);
        (3, Some No, None, 4);
        (4, None, None, 6);
        (5, None, None, 6);
        (6, None, None, 7);
        (7, None, Some ([{ 
            assumption = True; 
            conclusion = BoolExpr (LinExpr (1, "i", 0), Leq, LinExpr (1, "c", 1)) }]), 8)        
    ]
}

let cfg2 = {
    nodes = [
        (0, Start);
        (1, Statement (Assignment ("x", Constant 0)));
        (2, Statement (Read "k"));
        (3, Statement (Assignment ("a", Constant (-7))));
        (4, Statement (Assignment ("k", LinExpr (1, "k", (-3)))));
        (5, Branch (BoolExpr (Constant 128, Leq, LinExpr (1, "k", 0))));
        (6, Statement (Assignment ("b", Constant 0)));
        (7, Statement (Assignment ("k", LinExpr ((-1, "k", 0)))));
        (8, Statement (Assignment ("a", LinExpr (3, "a", 0))));
        (9, Statement (Assignment ("j", LinExpr (1, "a", 0))));
        (10, Statement (Write "j"));
        (11, Statement (Assignment ("j", Constant 2)));
        (12, Statement (Assignment ("a", Constant 4)));
        (13, Branch (BoolExpr (LinExpr (1, "j", 0), Eq, LinExpr (4, "a", 1))));
        (14, Statement (Assignment ("j", LinExpr (4, "a", 0))));
        (15, Statement (Write "j"));
        (16, Statement (Assignment ("j", LinExpr (1, "j", 1))));
        (17, Statement (Assignment ("a", LinExpr (3, "x", 5))));
        (18, Statement (Write "a"));
        (19, Statement (Assignment ("x", Constant 0)));
        (20, Join);
        (21, Statement (Assignment ("a", Constant 17)));
        (22, Statement (Assignment ("b", LinExpr (9, "x", 0))));
        (23, Join);
        (24, Statement (Assignment ("k", LinExpr (1, "k", (-4)))));
        (25, Statement (Write "b"));
        (26, Statement (Write "k"));
        (27, Stop);
    ];
    edges = [
        (0, None, None, 1);
        (1, None, None, 2);
        (2, None, None, 3);
        (3, None, None, 4);
        (4, None, None, 5);
        (5, Some No, None, 6);
        (6, None, None, 7);
        (7, None, None, 8);
        (8, None, None, 9);
        (9, None, None, 10);
        (10, None, None, 23);
        (5, Some Yes, None, 11);
        (11, None, None, 12);
        (12, None, None, 13);
        (13, Some No, None, 14);
        (14, None, None, 15);
        (15, None, None, 16);
        (16, None, None, 20);
        (13, Some Yes, None, 17);
        (17, None, None, 18);
        (18, None, None, 19);
        (19, None, None, 20);
        (20, None, None, 21);
        (21, None, None, 22);
        (22, None, None, 23);
        (23, None, None, 24);
        (24, None, None, 25);
        (25, None, None, 26);
        (26, None, Some ([
            {assumption=True; conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, LinExpr (1, "j", 0))};
            {assumption=True; conclusion=BoolExpr (LinExpr (2, "x", 0), Eq, LinExpr (1, "b", 0))};
            {assumption=True; conclusion=BoolExpr (LinExpr (7, "j", (-4)), Le, LinExpr (1, "k", 0))}
            ]), 27);
    ]
}

let cfg3 = {
    nodes = [
        (0, Start);
        (1, Statement (Assignment ("a", LinExpr (1, "a", (-3)))));
        (2, Statement (Assignment ("b", LinExpr (1, "b", 4))));
        (3, Statement (Assignment ("a", LinExpr (2, "a", 0))));
        (4, Statement (Assignment ("b", LinExpr (3, "b", 1))));
        (5, Statement (Assignment ("a", LinExpr (1, "a", 6))));
        (6, Statement (Assignment ("b", LinExpr (1, "b", (-13)))));
    ];
    edges = [
        (0, None, None, 1);
        (1, None, None, 2);
        (2, None, None, 3);
        (3, None, None, 4);
        (4, None, None, 5);
        (5, None, None, 6);
        (6, None, Some ([
            { assumption = True; conclusion = BoolExpr (LinExpr (1, "a", 0), Eq, Constant 0) };
            { assumption = True; conclusion = BoolExpr (LinExpr (1, "b", 0), Eq, Constant 0) }
        ]), 7);
    ]
}
