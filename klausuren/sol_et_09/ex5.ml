module type AddMul = sig
        type t
        val add : t -> t -> t
        val mul : t -> t -> t
end

module Integer : AddMul = struct
        type t = int
        let add a b = a+b
        let mul a b = a*b
end

module AddMulExpr (X : AddMul ) = struct
        open X
        type expr = Const of t
        | Var of string
        | Add of expr * expr
        | Mul of expr * expr

        let rec const e = match e with
                | Mul(l,r) -> let (l,r) = (const l, const r) in 
                        (match (l,r) with (Const l, Const r) -> Const (mul l r)
                        | _ -> Mul (l,r))
                | Add(l,r) -> let (l,r) = (const l, const r) in 
                        (match (l,r) with (Const l, Const r) -> Const (add l r)
                        | _ -> Add (l,r))
                | _ -> e

        let rec subst s n e = match e with
                | Mul(Var l, Var r) -> if l=s && r=s then Mul(n,n)
                        else if l=s then Mul(n, Var r)
                        else if r=s then Mul(Var l, n)
                        else e
                | Add(Var l, Var r) -> if l=s && r=s then Add(n,n)
                        else if l=s then Add(n, Var r)
                        else if r=s then Add(Var l, n)
                        else e
                | Var v when v=s -> n
                | _ -> e

       let rec simp e = match e with
                | Mul(l,r) -> let (l,r) = (simp l, simp r) in
                        (match (l,r) with (_, Add(s1,s2)) -> Add(Mul(l,s1),Mul(l,s2))
                        | (Add(s1,s2),_) -> Add(Mul(r,s1),Mul(r,s2))
                        | _ -> Mul(l,r))
                | Add(l,r) -> Add(simp l, simp r)
                | _ -> e

       let rec norm e = let r = simp e in
                if e=r then r else norm r
end
