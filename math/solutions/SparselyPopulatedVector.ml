type v = (int * int) list

let empty = []

let sb_vektor xs = let rec create i = function
        | [] ->  []
        | x::xs when x<>0 -> (i,x)::create (i+1) xs
        | x::xs -> create (i+1) xs
        in create 0 xs

let rec set i v = function [] -> if v<>0 then [(i,v)] else empty
        | (i',v')::xs -> if i'=i then 
                 if v<>0 then (i,v)::xs
                 else xs
                else if i<i' then 
                        if v<>0 then (i,v)::(i',v')::xs
                        else (i',v')::xs
                else (i',v')::set i v xs

let rec mul s v = if s=0 then empty else match v with
        | [] -> []
        | (i,v)::xs -> (i,s*v)::mul s xs

let rec add_sb_vektor v w = match (v,w) with
        | (_,[]) -> v
        | ([],_) -> w
        | ((vi,vv)::vt, (wi,wv)::wt) -> if(vi<wi) then (vi,vv)::add_sb_vektor vt w
                else if(vi>wi) then (wi,wv)::add_sb_vektor v wt
                else let r = vv+wv in 
                        if(r=0) then add_sb_vektor vt wt
                        else  (vi,r)::add_sb_vektor vt wt

let rec mul_sb_vektor v w = match (v,w) with
        | (_,[]) -> 0
        | ([],_) -> 0
        | ((vi,vv)::vt, (wi,wv)::wt) -> if(vi<wi) then mul_sb_vektor vt w
                else if(vi>wi) then mul_sb_vektor v wt
                else vv*wv+(mul_sb_vektor vt wt)
