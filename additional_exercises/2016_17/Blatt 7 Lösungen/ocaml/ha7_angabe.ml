type bin_tree =
    Node of bin_tree_node
  | Leaf
and bin_tree_node = { 
  key : int;
  left : bin_tree;
  right : bin_tree;
}
[@@deriving show]

let rec insert key = function
    Node n ->
    if key <= n.key then Node {n with left = insert key n.left}
    else Node {n with right = insert key n.right}
  | Leaf -> Node { key; left = Leaf; right = Leaf } 

type variable = { name : string }
and immediate =
    Bool of bool
  | Integer of int
and expression =
    Variable of variable
  | Immediate of immediate
  | BinOperation of binop
  | UnOperation of unop
  | Binding of { is_rec : bool; var : variable; expr : expression; inner : expression }
  | Func of { param : variable; body : expression }
  | Closure of { param : variable; body : expression }
  | FuncApp of { f : expression; param_act : expression }
  | IfThenElse of { cond: expression; then_branch: expression; else_branch: expression}
and binop_op =
    Addition
  | Subtraction
  | Multiplication
  | CompareEq
  | CompareLess
  | And
  | Or
and binop = binop_op * expression * expression
and unop_op =
    Negate
  | Not
and unop = unop_op * expression
and program = expression
[@@deriving show]

let add a b = BinOperation(Addition, a, b)
let sub a b = BinOperation(Subtraction, a, b)
let mul a b = BinOperation(Multiplication, a, b)
let _and a b = BinOperation(And, a, b)
let _or a b = BinOperation(Or, a, b)
let less a b = BinOperation(CompareLess, a, b)
let _eq a b = BinOperation(CompareEq, a, b)
let neg a = UnOperation(Negate, a)
let _not a = UnOperation(Not, a)
let var v = Variable ({name=v})
let imm i = Immediate (Integer i)
let _let v e inner = Binding {is_rec = false; var={name=v}; expr=e; inner=inner}
let _let_rec v e inner = Binding {is_rec = true; var={name=v}; expr=e; inner=inner}
let _fun p b = Func { param = {name=p}; body = b}
let _fun_xx p q b = _fun p (_fun q b)
let apply f p = FuncApp {f = f; param_act = p}
let apply_xx f p q = apply (apply f p) q
let ite c t e = IfThenElse { cond = c; then_branch = t; else_branch = e}
let _0 = var "@0"
let _1 = var "@1"
let _2 = var "@2"
let _3 = var "@3"