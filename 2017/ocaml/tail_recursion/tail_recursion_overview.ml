(* Was ist Endrekursion und warum brauchen wir es? *)
(* Betrachte zunächst die primitive Implementierung für die Fibonacci Zahlen *)
let rec fib n = let rec aux_fib n' = if n'=0 then (0,0) 
	else if n'=1 then (0,1)
	else let (pprev, prev) = aux_fib (n'-1) in (prev, pprev+prev)
	in snd (aux_fib n)

(* Ein Aufruf von fib 1000000 würde nun 1000000 rekursive Aufrufe benötigen. Da muss jedem
   Informatiker sofort das Herz aufschreien, denn unser Stack würde dann ebenfalls eine Tiefe 
   von mindestens 1000000 benötigen --> Stackoverflow/OutOfMemory exceptions.
   In imperativen Programmiersprachen können wir häufig Rekursion durch Verwendung von Schleifen vermeiden.
   Wie soll das aber in funktionalen Programmiersprachen funktionieren? 
   Die Antwort: Endrekursion. Wir schreiben die fib Funktion so um, dass der letzte Aufruf der Funktion der rekursive Aufruf ist.
   Somit kann der Compiler einfach den aktuellen Stackframe wiederverwenden und muss keinen neuen Rahmen auf den Stack erzeugen
   --> Effizient wie eine Schleife. *)
let fib_tail n = let rec fib_aux (pprev,prev) n' = if n'=n then pprev+prev 
	else fib_aux (prev,pprev+prev) (n'+1) in
	if n<=1 then n else fib_aux (0,1) 2
(* Wer mir nicht glaubt, dass das viel Unterschied macht, soll einfach mal fib 1000000 und fib_tail 1000000 laufen lassen :p *)

(* Achtung: Diese Funktion ist nicht endrekursiv! Der rekursive Aufruf von not_tail_rec ist zwar endrekursiv,
   jedoch verwendet die Funktion eine nicht endrekursive Funktion (fib)! *)
let rec not_tail_rec = function x when x<= 2 -> x
	| x -> let r = fib x in not_tail_rec (-r)

(* Diese offensichtliche nicht endrekursive Funktion... *)
let rec list_sum = function [] -> 0 | x::xs -> x+list_sum xs

(* ...kann durch eine neue Hilfsfunktion mit einem Akkumulator einfach endrekursiv gemacht werden. *)
let list_sum_tail l = let rec sum_aux acc = function [] -> acc 
	| x::xs -> sum_aux (acc+x) xs 
	in sum_aux 0 l
	
(* Eine absteigende Verzweigung*)
type 'a desc_twig = Twig of {data: 'a; sub: 'a desc_twig; stop: 'a} | End
(* Künstlerische Visualisierung für Twig {data=a; sub=Twig {data=b; sub=Twig {data=c; sub=End;stop=d};stop=e};stop=f}
        /a\
      /b\  f
    /c\  e
  END  d
*)

(* Wende rekursiv g zuerst von oben nach unten auf alle Sub-Zweige an und dann von unten nach oben auf alle Stop-Zweige.
   Also ergäbe sich als Reihenfolge für das obige Beispiel: a,b,c,d,e,f.
   Zunächst nicht endrekursiv: *)
let rec fold_twig g a = function End -> a
	| Twig {data=d;sub;stop=s} -> g (fold_twig g (g a d) sub) s

(* Und jetzt endrekursiv. Die Idee dabei ist, sich die noch ausstehenden Stop-Zweige in einer Liste zu merken.
   (Anmerkung: fold_left ist endrekursiv, fold_right allerdings nicht) *)
let fold_twig_tail g a t = let rec fold_aux acc rem = function 
	| End -> List.fold_left g acc rem
	| Twig {data=d;sub;stop=s} -> fold_aux (g acc d) (s::rem) sub
	in fold_aux a [] t

(* Alternative mit Continuations für die OCaml-Profis *)
type ('a,'b) cont = Cont of ('a -> ('a,'b) cont list -> 'b)

let fold_twig_conts g a t = let rec fold_aux acc conts = function 
	| End -> (match conts with (Cont x)::xs -> x acc xs | [] -> acc)
	| Twig {data=d;sub;stop=s} -> let c = Cont (fun acc conts -> fold_aux (g acc s) conts End) in
		fold_aux (g acc d) (c::conts) sub
	in fold_aux a [] t

(* Tipps, um Funktionen in endrekursive Funktionen abzuändern: 
   1) Definiere eine neue innere Hilfsfunktion, die einen Akkumulator übernimmt und das 
      Ergebnis im rekursiven Aufruf direkt berechnet, anstatt als letzte Operation im Aufrufer (vgl. list_sum und list_sum_tail). 
      Der Akkumulator kann nicht nur dazu dienen, bereits berechnete Werte zu speichern (vgl. list_sum_tail), sondern 
      auch, um sich noch ausstehende Operationen/Elemente zu speichern (vgl. fold_twig_tail)
   2) Versuche rekursive Berechnungen bottom-up anstatt top-down zu berechnen (vgl. fib und fib_tail) *)
