let todo _ = failwith "TODO something is not implemented"

open Event

let ch_faster = todo

let ch_slower = todo

let ch_both  = todo

type w = Send of string
type r = Fetch | Receive of string

let start_box = todo

(*Tests*)
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

(*Nachricht an die Box senden*)
let _ = sync(send wc (Send "Meine Nachricht"))
(*Kommando, dass ich eine Nachricht von der Mailbox will*)
let _ = sync(send rc Fetch)
(*Und nun kann ich auf die Antwort warten*)
let _ = match sync(receive rc) with Receive a -> print_string a
