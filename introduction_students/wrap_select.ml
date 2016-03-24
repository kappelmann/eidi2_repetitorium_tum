(*Select nimmt das schneller Event von einer Liste aus Events
 * val select : 'a event list -> 'a *)
let faster = select [e1;e2]

(*Wrap packt um das übergebene Event im ersten Parameter ein neues Event, indem es
 * auf das Ergebnis es ersten Events die übergebene Funktion anwendet
 * val wrap : 'a event -> ('a -> 'b) -> 'b event *)

(*Der schnellere gewinnt*)
let s = select [ 
                wrap (receive c_odd) (fun a -> ("odd",a)) ;
                wrap (receive c_even) (fun a -> ("even",a))
               ]
(*oder äquivalent*)
let s = choose [ 
                wrap (receive c_odd) (fun a -> ("odd",a));
                wrap (receive c_even) (fun a -> ("even",a))
               ] |> sync

(* val choose : 'a event list -> 'a event*)

(*So ist select im Events module definiert*)
let select y = sync(choose y)

(*Beide Ergebnisse werden empfangen. Zuerst vom schnelleren, dann vom langsamen*)
let s = select [ 
           wrap(receive c_odd) (fun a -> (a, sync(receive c_even)));
           wrap(receive c_even) (fun a -> (sync(receive c_odd), a))
           ]
