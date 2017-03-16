open Lists
open ColUtils

open NativeList
module NativeListUtils = ColUtils(NativeList)

let e = empty
let e = add 4 e
let e = add 7 e |> add 10 |> add 123
let expected =  add 123 empty |> add 7
let res = NativeListUtils.filter (fun a -> (a mod 2)=1) e
let b = res=expected
let _ = if not b then failwith "Sadly, your filter is broken!"

let e = add "OCaml ist voll supe" empty |> add "whateve"
let expected = add "whatever" empty |> add "OCaml ist voll super"
let res = NativeListUtils.for_all (fun a -> a^"r") e
let b = res=expected && b
let _ = if not b then failwith "Sadly, your for all is broken!"
let b = NativeListUtils.length e = 2 && b
let _ = if not b then failwith "Sadly, your length broken!"

open MyList
module MyListUtils = ColUtils(MyList)

let e = add 7.0 empty |> add 323.0 |> add 3023.123 |> add 1337.0
let res = MyListUtils.exists (fun a -> a>3000.0) e
let expected = Some 3023.123
let b = res=expected && b
let _ = if not b then failwith "Sadly, your exists is broken!"
let res = MyListUtils.exists (fun a -> a=3000.0) e
let expected = None
let b = res=expected && b
let _ = if not b then failwith "Sadly, your exists is broken!"
let b = MyListUtils.length e = 4 && b
let _ = if not b then failwith "Sadly, your length is broken!"
let res = MyListUtils.merge e e
let b = MyListUtils.length res = (MyListUtils.length e * 2) && b
let _ = if not b then failwith "Sadly, your merge is broken!"
let res = MyListUtils.tuple e (add "last" empty |> add "third" |> add "second" |> add "first")
(*helper to debug result*)
(*let _ = MyListUtils.for_all (fun (l,r) -> let _ = print_float l; print_string r in (l,r)) res*)
let expected = add (7.0,"last") empty |> add (323.0,"third") |> add (3023.123,"second") |> add (1337.0,"first")
let expected2 =  add (1337.0,"first") empty |> add (3023.123,"second") |> add (323.0,"third") |> add (7.0,"last")
let b = (res = expected || res=expected2) && b
let _ = if not b then failwith "Sadly, your tuple is broken!"
let _ = try MyListUtils.tuple e empty with MyListUtils.Different_Length -> empty | _ -> failwith "Sadly, your tuple is broken!"
let res = MyListUtils.every_nth e 2 
let expected = add 3023.123 empty |> add 7.0
let b = res=expected && b
let _ = if not b then failwith "Sadly, your every_nth is broken!"
let res = MyListUtils.every_nth e 0
let expected = empty
let b = res=expected && b
let _ = if not b then failwith "Sadly, your every_nth is broken!"

let _ = if b then print_string "Your functors are as awesome as OCaml! ;)" else failwith "Sadly, your functors are broken!"
