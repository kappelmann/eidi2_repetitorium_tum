let todo = failwith "Todo implement"

open Event

(*Creates a thread that filters the list and returns the new filtered list
 * Parameters
 * p: predicate for the filter
 * l: the list which should be filtered
 * c: the channel where the value should be returned to
 * returns: unit *)
let threaded_filter p l c = todo 

let print_and_return s = print_string s; s
(*Alternative*)
let print_and_return s = let _ = print_string s in s

let c_odd = new_channel ()
let c_even = new_channel ()

let odd n = (n mod 2) = 1
let even n = not (odd n)
let l = [1;2;3;4;5]
let _ = threaded_filter odd l c_odd
let _ = threaded_filter even l c_even
let s1 = sync(receive c_odd)
let s2 = sync(receive c_even)
