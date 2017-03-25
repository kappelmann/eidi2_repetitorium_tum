open Collection

module ColUtils (C: Collection) = struct
        let filter p a = C.fold (fun c a -> if p a then C.add a c else c) C.empty a
        let for_all f a = C.fold (fun c a -> C.add (f a) c) C.empty a
        let length a = C.fold (fun c a -> c+1) 0 a
        let exists p a = C.fold (fun c a -> if(c=None && p a) then Some a else c) None a
        let merge a b = C.fold (fun c a -> C.add a c) b a
        exception Different_Length
        let tuple a b = if length a <> length b then raise Different_Length else 
                let get_outer c = let (_,Some l) = C.fold (fun (c',l) b -> if c'=c then (c'+1,Some b) 
                        else ((c'+1),l)) (0,None) b 
			in l
                in let (_,l) = C.fold (fun (c,l) inner -> (c+1, C.add (inner, get_outer c) l)) (0, C.empty) a 
       		in l
        let every_nth a n = let n=n-1 in let (_,l) = C.fold (fun (c,l) a -> if c=n then (0,C.add a l) 
                else (c+1,l)) (0,C.empty) a
		in l
end
