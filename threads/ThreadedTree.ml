type 'a tree = Leaf of 'a | Node of ('a tree * 'a tree)

let min t = let open Event in let open Thread in 
        let rec search c = function Leaf v -> sync(send c v)
                | Node(l,r) -> let (lc,rc) = (new_channel (),new_channel ()) in 
                        let _ = (create (search lc) l, create (search rc) r) in
                        let (lv,rv) = (sync(receive lc), sync(receive rc)) in
                        sync(send c (if lv<rv then lv else rv))
        in
        let c = new_channel () in 
        let _ = create (search c) t in sync(receive c)
