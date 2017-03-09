let todo _ = failwith "TODO something is not implemented"

open Event

let ch_faster e1 e2 = select [e1;e2]

let ch_slower e1 e2 = select [wrap e1 (fun _ -> sync e2);
        wrap e2 (fun _ -> sync e1)]

let ch_both e1 e2 = select [wrap e1 (fun x -> (x,sync e2));
        wrap e2 (fun x -> (sync e1, x))]

type w = Send of string
type r = Fetch | Receive of string

let start_box () = let (wc,rc) = (new_channel(),new_channel()) in 
        let rec loop m = 
                let m = select [
                wrap(receive wc) (fun (Send a) -> a::m);
                wrap(receive rc) (fun x -> if x<>Fetch then m else
                        match m with 
                        | [] -> sync(send rc (Receive "empty")); []
                        | x::xs -> sync(send rc (Receive x)); xs )
                ] in 
                loop m in 
        let _ = Thread.create loop [] in
        (wc,rc)

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
