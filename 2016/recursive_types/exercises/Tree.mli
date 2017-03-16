open Collection

module type Tree = sig
        include Collection
        val to_list : 'a t -> 'a list
        val merge : 'a t -> 'a t -> 'a t
        (*Throw an exception if the tree is empty*)
        val min : 'a t -> 'a
        (*Return all elements that are greater than the passed element*)
        val greater_than : 'a t -> 'a  -> 'a t
end
