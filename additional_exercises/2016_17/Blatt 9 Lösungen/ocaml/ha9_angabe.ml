type 'a row = 'a
[@@deriving show]
type 'a csv = 'a row list
[@@deriving show]

type 'a setter = string -> 'a row -> 'a row
type ('a, 'b) getter = 'a row -> 'b

exception Column_count_does_not_match
exception Invalid_decoration of string
exception Could_not_decorate