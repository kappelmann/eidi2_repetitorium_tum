open Event

type 'a t = 'a channel
let create f v = let c = new_channel() in
        let rec calc v = sync(send c (f v)); calc v in
        let _ = Thread.create calc v in
        c
        
let get c = sync(receive c)
