Thread.create <function> <parameter für function> und gibt eine Threadid zurück, d.h.:
('a -> 'b) -> 'a -> t

Thread.create (fun x -> x+1) 10

let t = fold (fun a x -> a+x) 0
Thread.create t [1;2;3]
oder
Thread.create (fold (fun a x -> a+x) 0) [1;2;3]

Aufgabe
1) Funktion, die eine int liste nimmt und alle Elemente auf die Konsole
   ausgibt
2) Nebenläufig die Funktion 1) in einen Thread ausführen

Printen: print_string, print_int, print_float ....

let rec f = function [] -> print_string "\n"
	| x::xs -> print_int x ; print_string ", "; f xs

let _ = Thread.create f [1;2;3]

Kompilieren:
ocamlc -o test.out -thread -I +threads unix.cma threads.cma <deinProgramm>

Events:

open Event

let c = new_channel ()
let a = Thread.create (fun ch -> sync(send ch "hallo")) c
let s = sync (receive c)

(* würde für immer blockieren *)
(* let s2 = sync (receive c) *)

let _ = print_string s

let c = new_channel ()
let t1 = Thread.create (fun ch -> sync(send ch "Thread 1")) c
let t2 = Thread.create (fun ch -> Thread.delay 1; sync(send ch "Thread 2")) c
(*Zuerst vom schnellern Thread empfangen*)
let s = sync (receive c)
(*Aber danach auch vom langsamen Thread*)
let s = (s, sync(receive c))
