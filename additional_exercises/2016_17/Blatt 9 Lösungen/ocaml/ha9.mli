open Ha9_angabe

val read_csv : 'a row -> 'a setter list -> string -> 'a csv
val group_by : 'a csv -> ('a, 'b) getter -> 'a row list list
val write_xmas : string -> string -> int -> unit
