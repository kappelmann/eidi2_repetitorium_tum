open Lists
open NativeList

let e = empty
let e = add 4 e
let e = add 1990 e
let e = add 10000 e |> add 10000
let expected = 25021994
let res = fold (fun a b -> a+b) 25000000 e
let b = res=expected
let _ = if b then print_string "Congratulations, you finished your NativeList. Now try your MyList collection.\n" else failwith "Your NativeList failed its job. :("

open MyList

let e = empty
let e = add "sentence." e |> add "correct " |> add "make a " |> add "should "
let expected = "This should make a correct sentence."
let res = fold (fun a b -> a^b) "This " e
let b = res=expected
let _ = if b then print_string "Wow, your MyList also works! +5 exam preparation points for you!\n" else failwith "Your MyList implementation failed its job. :("
