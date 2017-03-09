exception MyException;;
exception ParException of string
let f a = if a=0 then raise MyException else if a=1 then raise (ParException "failed") else a
let e = f 0 (*throws MyException*)
let e = try f 0 with MyException -> 10 | ParException _ -> 11
(*e is now 10*)
let e = try f 1 with MyException -> 10 | ParException _ -> 11
(*e is now 11*)
let e = try f 0 with _ -> 12
(*e is now 12*)
