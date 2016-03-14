exception MyException;;
exception ParException of string
let f a = if a=0 then raise MyException else if a=1 then raise (ParException "failed") else a
let e = f 0 (*Wirft eine MyException*)
let e = try f 0 with MyException -> 10 | ParException _ -> 11
(*e ist jetzt 10*)
let e = try f 1 with MyException -> 10 | ParException _ -> 11
(*e ist jetzt 11*)
let e = try f 0 with _ -> 12
(*e ist jetzt 12*)

