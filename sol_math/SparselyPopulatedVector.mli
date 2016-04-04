(* Vektoren von ganzen Zahlen lassen sich in OCaml als Listen von int repräsentieren. Als schwach
 * besetzt bezeichnet man Vektoren, die einen hohen Anteil von Nullelementen aufweisen. In sol-
 * chen Fällen kann es sinnvoll sein, diese Vektoren als Listen von (int * int)-Tupeln zu spei-
 * chern, wobei die erste Zahl die Position im Vektor und die zweite Zahl den eigentlichen Wert
 * angibt. Es ist dann nicht mehr nötig, die Nullwerte zu speichern. 
 *
 * Beispiel:
 * Darstellung eines Vektors als Liste von int: [1;0;5;0;0;6]
 * Darstellung des Vektors ohne Nullelemente: [(0,1);(2,5);(5,6)] *)

(* Im folgenden sind die Namen der zu implementierenden Funktionen und typen vorgegeben.
 * Fehlende Parameter müssen selbstständig ergänzt werden. Ergänzen Sie zunächst
 * die Signaturen in dieser .mli-Datei und anschließend die Implementierung in der
 * zugehörigen .ml-Datei.*)

(* Der Typ eines schwach besetzten Vektors, der integer aufnimmt.*)
type v = (int * int) list

(* Liefert einen leeren schwach besetzten Vektor.*)
val empty : v

(* Transformiert einen Vektor, der als int list repräsentiert ist,
 * in die oben definierte Form für schwach besetzte Vektoren.*)
val sb_vektor : int list -> v

(* Setzt den Wert an der gegebenen Stelle des gegebenen Vektors auf den gegebenen Wert.
 * Erster Parameter: Position
 * Zweite Parameter: neuer Wert
 * Dritter Parameter: Vektor*)
val set : int -> int -> v -> v

(* Multipliziert einen gegebenen Vektor mit einen gegebenen integer Skalarfaktor.
 * Erster Parameter: Skalarfaktor
 * Zweiter Parameter: Vektor*)
val mul : int -> v -> v

(* Addiert zwei schwach besetzte Vektoren.*)
val add_sb_vektor : v -> v -> v

(* Skalarprodukt zweier schwach besetzten Vektoren.
 * Erinnerung: Das Skalarprodukt zweier Vektoren v=(v1,v2,..,vn)
 * und w=(w1,w2,..,wn) ist definiert als <v,w>=v1*w1+v2*w2+...+vn*wn *)
val mul_sb_vektor : v -> v -> int

(*Anm.: Sparsely populated = schwach besetzt*)
