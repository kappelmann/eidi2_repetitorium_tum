type s = Student of string
type p = float
type k = Klausur of s*(p list)
type g = Eins | Zwei | Drei | Vier | Fuenf
(*Die Notenskala weißt jeder Note einen oberes und unteres
 *Punktelimit zu*)
type n = Skala of (g*p*p) list

(*Berechnet die Summe der Punkte einer Klausur*)
val sum_k : k -> p
(*Ermittelt die Note für eine gegebene Punkteanzahl mit dem gegebenen Notenspiegel.
 Sollte keine passende Note gefunden werden, wird eine Exception geworfen*)
val get_grade : p -> n -> g
(*Berechnet die Note für eine Klausur mit dem gegebenen Notenspiegel*)
val grade_k : k -> n -> g
(*Berechnet für eine Liste von Klausuren mit gegebenen Notenspiegel
 die Liste von Noten für die Studenten. 
 Da die Übungsleitung viele Tutoren besitzt, wird die Klausurenliste komplett
 parallelisiert benotet, d.h. die Note jeder Klausur wird immer von einem 
 eigenen Thread berechnet. Schneller Threads sollen dabei nicht auf langsamere
 Threads warten müssen.*)
val eval : k list -> n -> (s*g) list
