open Event

(* Aufgabe 1: threaded_filter erstellt einen Thread, der eine Liste begzüglich eines Prädikates filtert und 
   das Ergebnis über einen Kanal zurücksendet.
 * Parameter:
 *   Das Prädikat
 *   Die zu filternde Liste
 *   Der Channel, auf dem das Ergebnis zurückgegeben soll
 * Rückgabetyp: unit *)
(* Geben sie die passende Signatur für die Funktion an *)
val threaded_filter : ('a -> bool) -> 'a list -> 'a list channel -> unit

(* Aufgabe 2: ’a t soll die asynchrone Berechnung eines Werts vom Typ ’a darstellen. create f a be-
   kommt die Funktion f mit dem Argument a und gibt ein Future zurück, das f a asynchron
   berechnet. get f blockiert bis das Ergebnis des Futures f berechnet wurde und gibt dieses
   dann zurück. Achten Sie darauf, dass folgende Aufrufe weiterhin das Ergebnis liefern und
   nicht blockieren! *)
type 'a t
val create : ('a -> 'b) -> 'a -> 'b t
val get : 'a t -> 'a

(* Aufgabe 3: Ein Baum soll Informationen ausschließlich in den Blättern speichern.
   Innere Knoten speichern nur jeweils ihr linkes und rechtes Kind.
   Der Baum soll ungeordnet sein. Insbesondere muss das minimale bzw. maximale Element
   nicht unbedingt ganz links bzw. ganz rechts stehen. *)
type 'a tree = Leaf of 'a | Node of ('a tree * 'a tree)

(* Sucht nach dem kleinsten Wert eines übergebenen Baumes. Für jeden inneren Knoten soll die Suche
   rekursiv jeweils in einen neuen Thread pro Kind durcheführt werden. *)
(* Geben Sie die passende Signatur für die Funktion an *)
val min : 'a tree -> 'a



