type 'a tree = Leaf of 'a | Node of ('a tree * 'a tree)

let rec min t = 
        let open Event in let open Thread in
        let help c t = sync(send c (min t)) in
        match t with Leaf v -> v
        |Node (l,r) -> let (cl,cr) = new_channel() in
                        let _ = Thread.create (help cl) l in
                        let _ = Thread.create (help cr) r in
                        let lv = sync(receive cl) in
                        let rv = sync(receive cr) in
                        if lv<rv then lv else rv
