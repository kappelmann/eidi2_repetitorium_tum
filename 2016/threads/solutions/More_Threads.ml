open Event

let rec map f = function [] -> []
        | x::xs -> f x::map f xs

let tmap f l = let rec help = function [] -> []
        | x::xs -> let c = new_channel ()
          in let _ = Thread.create (fun c -> sync (send c (f x))) c 
          in (receive c)::help xs
        in map sync (help l)

(*in den integer sind immer die belegten Plätze angegeben*)
type w = Waggon of int * w | End
type z = Zug of int * w

let rec zcount (Zug(i',wl)) = match wl with 
        | End -> i'
        | Waggon(i,wl) -> zcount (Zug (i+i',wl))

let rec sum = function [] -> 0
        | x::xs -> x+sum xs

let rec length = function [] -> 0
        | _::xs -> 1+length xs

let avg_tzcount l = let zl = length l in
        let el = tmap zcount l in
        (float_of_int(sum el)) /. ((float_of_int)zl)

let rec find p = function [] -> None
        | x::xs -> if p x then (Some x) else find p xs

(* Nicht ganz perfekt, da man auf alle Threads warten muss. 
   Wenn man allerdings das Ergebnis des schnellsten Threads direkt 
   verwendet, terminieren die anderen Threads nie, da ihr sync (send <data>) nie
   einen Empfänger findet. *)
let tfind p l = let fl = tmap (find p) l in 
        let rec find_some = function [] -> None
                | x::xs -> if x<>None then x else find_some xs
        in find_some fl

let rec tfib = function n when n<=1 -> n
        | n -> let (c1,c2) = (new_channel(),new_channel()) in
               let csend c n = sync(send c (tfib n)) in
               let _ = Thread.create (csend c1) (n-1) in
               let _ = Thread.create (csend c2) (n-2) in
               let sr c = sync(receive c) in
               let (v1,v2) = ((sr c1),(sr c2)) in 
               v1+v2

let ch_faster e1 e2 = select [e1;e2]

let ch_slower e1 e2 = select [wrap e1 (fun _ -> sync e2);
        wrap e2 (fun _ -> sync e1)]

let ch_both e1 e2 = select [wrap e1 (fun x -> (x,sync e2));
        wrap e2 (fun x -> (sync e1, x))]

let ch_slowest el = let l = length el in
	if l=0 then None else
        let rec aux i = let res = select el in
                if i=l then Some res else aux (i+1)
        in aux 1

let tcalc f l = if (length f)<>(length l) then None
        else if f=[] then None
        else let rec create f l = match (f,l) with
                | ([],[]) -> []
                | (f::fs, x::xs) -> let c = new_channel () in
                        let _ = Thread.create (fun x -> sync(send c (f x))) x
                        in receive c::create fs xs
        in Some (select (create f l))

type s = Send of string
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
