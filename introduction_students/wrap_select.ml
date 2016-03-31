(*Select nimmt das schnellste Event von einer Liste aus Events
 * val select : 'a event list -> 'a *)
let faster = select [e1;e2]
(* val choose : 'a event list -> 'a event*)
let faster_event = choose [e1;e2]
(*So ist select im Events module definiert*)
let select y = sync(choose y)

(* Wrap packt um das übergebene Event im ersten Parameter ein neues Event, indem es
 * auf das Ergebnis es ersten Events die übergebene Funktion anwendet
 * val wrap : 'a event -> ('a -> 'b) -> 'b event *)

(*Create two channels*)
let (c_odd,c_even) = (new_channel(), new_channel())

(*Der schnellere gewinnt*)
let s = select [(receive c_odd);(receive c_even)]

let (n,v) = select [ 
                wrap (receive c_odd) (fun a -> ("odd",a)) ;
                wrap (receive c_even) (fun a -> ("even",a))
               ]
let _ = print_string n
let s = v
(*oder äquivalent*)
let s = choose [ 
                wrap (receive c_odd) (fun a -> ("odd",a));
                wrap (receive c_even) (fun a -> ("even",a))
               ] |> sync


let _ = Thread.create (fun x -> Thread.delay 100; sync(send c_odd x)) 1
let _ = Thread.create (fun x -> sync(send c_even x)) 2

(*Beide Ergebnisse werden empfangen. Zuerst vom schnelleren, dann vom langsamen*)
let (c_odd_val, c_even_val) = select [
           wrap (receive c_odd) (fun a -> (a, sync(receive c_even)));
           wrap (receive c_even) (fun a -> (sync(receive c_odd), a))
           ]

let (c_odd_val, c_even_val) = (sync(receive c_odd), sync(receive c_even))
