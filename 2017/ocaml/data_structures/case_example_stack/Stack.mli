module type Stack = sig
        type 'a stack
        val empty : 'a stack
        val is_empty : 'a stack -> bool
        val push : 'a stack -> 'a -> 'a stack
        val pop : 'a stack -> ('a option * 'a stack)
end
