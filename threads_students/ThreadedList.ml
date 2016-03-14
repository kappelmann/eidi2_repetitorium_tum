let todo _ = failwith "Todo implement"
let c_odd = new_channel ()
let c_even = new_channel ()
(*Creates a thread that filters the list and returns the new filtered list*)
(*Parameters
 * p: predicate for the filter
 * l: the list which should be filtered
 * c: the channel where the value should be returned to
 * returns: unit *)
let threaded_filter p l c = todo

let odd n = n mod 2 = 1
let even = not odd
let _ = threaded_filter odd [1;2;3;4;5] c_odd 
let _ = threaded_filter even [1;2;3;4;5] c_even
(*Der schnellere gewinnt*)
let s = select [ 
                wrap(receive c_odd) (fun a -> a)
                wrap(receive c_even) (fun a -> a)
               ]
(*oder äquivalent*)
let s = choose [ 
                wrap(receive c_odd) (fun a -> a)
                wrap(receive c_even) (fun a -> a)
               ] |> select

(*Beide Ergebnisse werden empfangen*)
let s = select [ 
           wrap(receive c_odd) (fun a -> (a, sync(receive c_even)))
           wrap(receive c_even) (fun a -> (sync(receive c_odd), a))
           ]
