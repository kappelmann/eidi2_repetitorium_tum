type s = Student of string
type p = float
type k = Klausur of s*(p list)
type g = Eins | Zwei | Drei | Vier | Fuenf
(*Die Notenskala weißt jeder Note einen oberes und unteres
 *Punktelimit zu*)
type n = Skala of (g*p*p) list

let rec sum_k (Klausur (s,l)) = let rec sum = function 
        | [] -> 0.0
        | x::xs -> x+.sum xs 
        in sum l

let rec get_grade p = function Skala [] -> raise (Failure "no grade found")
        | Skala ((g,o,u)::xs) -> if(p>=o&&p<=u) then g 
                else get_grade p (Skala xs)

let grade_k k n = get_grade (sum_k k) n

let eval kl n = let open Event in let open Thread in
        let calc k c = sync(send c (grade_k k n)) in
        let wrap_c (Klausur (s,l)) c = wrap (receive c) (fun g -> (s,g)) in
        let tl = List.map (fun k -> let c = new_channel() 
                in let _ = Thread.create (calc k) c
                in wrap_c k c) kl
        in List.map (fun x -> Event.select tl) tl
