open BinTree.BinTree

let e = empty
let e = add 4 e
let e = add 1990 e
let e = add 4 e
let e = add 10000 e |> add 10000
let e = add (-1) e
let expected = 25011993
let res = fold (fun a b -> a+b) 25000000 e
let b = res=expected
let _ = if not b then failwith "Your tree seems to have problems with the add function."
let _ = if (-1)<>min e then failwith "Your tree delivered the wrong min. element"
let b = empty |> add 1 |> add 10000 |> add 0 |> add (-10)
let e = merge e b
let expected = 25011984
let res = fold (fun a b -> a+b) 25000000 e
let b = res=expected
let _ = if not b then failwith "Your tree seems to have problems with the merge function."
let _ = if (-10)<>min e then failwith "Your tree delivered the wrong min. element"
let t = e
let e = greater_than e 9999
let expected = 10000
let res = fold (fun a b -> a+b) 0 e
let b = res=expected
let _ = if not b then failwith "Your tree seems to have problems with the greater_than function."
let e = greater_than e 10000
let expected = 0
let res = fold (fun a b -> a+b) 0 e
let b = res=expected
let _ = if not b then failwith "Your tree seems to have problems with the greater_than function."
let e = greater_than t 0
let expected = 11995
let res = fold (fun a b -> a+b) 0 e
let b = res=expected
let e = empty |> add "Strings are causing problems with my tree :(" |> add "My tree can handle generic types"
let _ = print_string (min e^"\n")
let _ = if b then print_string "Your trees are awesome! +5 exam preparation points for you!" else failwith "Your tree seems to have problems with the greater_than function."
