open Event
open Thread

let ch2 = new_channel ()

let serve f = 
 let ch = new_channel () 
 in let rec aux () = 
   (* Anfrage empfangen *)
   let r = sync @@ receive ch in 
   (* Neuen Thread, der die Anfrage mit f berechnet, starten. Ausserdem soll dieser das Ergebnis zurueckschicken. *)
   let _ = Thread.create (fun x -> sync @@ send ch2 @@ f x) r 
   (* Rekursiv wieder Anfrage empfangen *)
   in aux ()
 in let _ = Thread.create aux ()
 in ch

 (* Anfrage an loopenden Thread abschicken *)
let request ch a = let _ = sync @@ send ch a in
        (* Ergebnis vom vom loopenden Thread erstellten Thread empfangen *)
        sync @@ receive ch2

let f x = f+10
let ch = serve f
let _ = Thread.create (fun x -> request ch x) 10
let c = request ch 2
