(*Kompilieren mit Threadunterstützung:*)
(*ocamlc -o test.out -thread -I +threads unix.cma threads.cma <deinProgramm>*)

(* Natürlich beherrscht OCaml auch Threading.
   Ein Thread wird erzeugt durch Thread.create <function> <parameter für function>.
   Der Thread berechnet dann die übergeben Funktion mit dem übergebenen Parameter und terminiert danach.
   Thread.create gibt eine Thread-ID des erstellten Threads zurück, die man z.B. verwenden kann, um den erzeugten Thread zu killen.
   Die Signatur lautet ('a -> 'b) -> 'a -> t *)

(* Beispiel: Erstelle einen Thread, der seinen Parameter mit 1 addiert
   wir übergeben dem neuen Thread die Zahl 10 *)
let t = Thread.create (fun x -> x+1) 10
(* Beachte: t enthält nicht jetzt nicht das Ergebnis der Berechnung des Threads 11, sondern die Thread-ID.
   Wie wir die Kommunikation zwischen den Threads herstellen können, also z.B. hier das Ergebnis 11 zurückerhalten, kommt später. *)

(* Anderes Beispiel: Fold in einem neuen Thread*)
Thread.create (fold (fun a x -> a+x) 0) [1;2;3]
(* Alternativ hätten wir auch schreiben können: *)
Thread.create (fun () -> fold (fun a x -> a+x) 0 [1;2;3]) ()
(* Die Klammern () entsprechen was man aus Java als void kennt. In OCaml
   haben sie den Typ unit. *)

(* Aufgabe:
 * 1) Erstelle eine Funktion, die eine int liste nimmt und alle Elemente auf die Konsole
 * ausgibt
 * 2) Führe die Funktion aus 1) nebenläufig in einen Thread aus.
 * Print-Funktionen: print_string, print_int, print_float, ...*)

(*Lösung Aufgabe 1) *)
let rec f = function [] -> print_string "\n"
	| x::xs -> print_int x ; print_string ", "; f xs
(*Lösung fuer Aufgabe 2*)
let _ = Thread.create f [1;2;3]
(* Der Infix-Operator ; nimmt zwei Ausdrücke und gibt das Ergebnis des rechten Ausdruckes zurück.
   z.B. liefert (1+2);(2+3) die Zahl 5 zurück. Im Normalfall sollte der linke Term einen unit zurückliefern. *)

open Event
(* Nun widmen wir uns dem Datenaustausch zischen Threads. Dies geschieht mit channels, die man
   sich wie eine Datenleitung zwischen Threads vorstellen kann. Die notwendigen Funktionen (new_channel, sync, send, receive, wrap, select)
   befinden sich im Event Modul. 
   Zunächst erstellen wir uns einen channel: *)
let ch = new_channel ()
(* Jetzt wollen wir einen neuen Thread starten, und auf dessen Ergebnis warten 
   Dafür erstellen wir einen Thread, der uns ganz primitv nur die Zahl 10 zurücksenden soll.
   Das senden passiert über den Channel mit Hilfe von send und sync. 
   send <channel> <data> liefer ein sogenannten Event zurück. Auf dieses können wir uns dann mit sync <event> synchronisieren, um
   die Daten, die im Event stecken, wirklich loszusenden. Das kann man sich so vorstellen, als ob
   wir uns zunächst nur vorbereiten, Daten über die Leitung zu schicken, aber noch nicht wirklich lossenden.
   Erst mit sync senden wir die Nachricht los UND blockieren danach solange, bis jemand unsere Nachricht vom Kanal abnimmt. *)
let _ = Thread.create (fun c -> sync(send c 10)) ch
(* Analog verläuft es mit dem Empfang von Daten. mit receive <channel> bereiten wir uns zunächst nur 
   aufs Empfangen vor und mit sync <event> warten wir solange, bis wir Daten vom Kanal erhalten *)
let e = sync (receive ch)
let _ = print_int e
let ch = new_channel ()
(* Würden wir jetzt erneut sync (receive ch) aufrufen, erhalten wir nicht erneut die Zahl 10, sondern
   würden für immer blockieren/warten. *)

(* Es können auch mehrere Threads über einen Kanal senden *)
let t1 = Thread.create (fun ch -> sync(send ch "Thread 1")) c
let t2 = Thread.create (fun ch -> Thread.delay 1; sync(send ch "Thread 2")) c
(* Zuerst vom schnellen Thread empfangen *)
let s = sync (receive c)
(* Aber danach auch vom langsamen Thread *)
let s = (s, sync(receive c))
