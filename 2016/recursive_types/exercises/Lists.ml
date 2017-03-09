open Collection
let todo  = failwith "Ups, there is still something not implemented."

(*An implementation that uses the list implementation provided by OCaml*)
module NativeList : Collection = struct
        type 'a t = todo
        let empty = todo 
        let add p l = todo
        (*Use a tail recursive fold version!*)
        let rec fold f a l = todo
end

(*An implementation that does not use OCaml lists*)
module MyList : Collection = struct
        type 'a t = todo
        let empty = todo
        let add p l = todo
        let rec fold f a l = todo
end
