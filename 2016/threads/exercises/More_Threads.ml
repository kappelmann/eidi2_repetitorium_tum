let todo = failwith "Todo not implemented"

let tmap = todo

(* in den integer sind immer die belegten Plätze angegeben *)
type w = Waggon of int * w | End
type z = Zug of int * w

let zcount = todo

let avg_tzcount = todo

let tfind = todo

let tfib = todo

let ch_faster = todo

let ch_slower = todo

let ch_both  = todo

let ch_slowest = todo

let tcalc = todo

type s = Send of string
type r = Fetch | Receive of string

let start_box = todo

(* Tests *)
let (wc,rc) = start_box ()
let print = function Receive a -> print_string a; print_string "\n" 
                     | _ -> raise (Failure "illegal message")
let receive () = sync(send rc Fetch); sync(receive rc)

let _ = sync(send wc (Send "My magnific mailbox makes me marvel!"))
let a = receive ()
(*Should print My magnific mailbox makes me marvel!*)
let _ = print a
let a = receive ()
(*Should print empty*)
let _ = print a
