type node = int
type graph = (node * node list) list

(*o=outgoing, i=incoming node of the new edge
 *we insert everything sorted *)
let rec add_edge g o i = let rec insert_node i = function [] -> [i]
| x::xs -> if x<i then x::insert_node i xs
	else if x>i then i::x::xs
	else x::xs
in 
match g with [] -> [(o,[i])]
| (o',l)::xs -> if o'<o then (o',l)::add_edge xs o i
	else if o'>o then (o,[i])::g
	else (o,insert_node i l)::xs

(*todo: Rest der Aufgaben*)
