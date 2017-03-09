module MyMap :
  sig
    type ('k, 'v) t = Empty | Node of 'k * 'v * ('k, 'v) t * ('k, 'v) t
    val empty : ('k, 'v) t
    val to_list : ('k, 'v) t -> ('k * 'v) list
    val show : ('k -> string) -> ('v -> string) -> ('k, 'v) t -> string
    val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
    val from_list : ('k * 'v) list -> ('k, 'v) t
    val find : 'k -> ('k, 'v) t -> 'v option
    val mem : 'k -> ('k, 'v) t -> bool
    val min : ('k, 'v) t -> ('k * 'v) option
    val max : ('k, 'v) t -> ('k * 'v) option
    val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
    val map : ('v1 -> 'v2) -> ('k, 'v1) t -> ('k, 'v2) t
    val filter : ('k * 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t
    val merge : ('k -> 'v1 option -> 'v2 option -> 'v3 option) -> ('k, 'v1) t -> ('k, 'v2) t -> ('k, 'v3) t
  end
module Json :
  sig
    type t =
        Null
      | Bool of bool
      | Number of float
      | String of string
      | Object of (string, t) MyMap.t
      | Array of t list
    type offset = Field of string | Index of int
    type path = offset list
    val show : t -> string
    val map : (t -> t) -> t -> t
    val get_offset : offset -> t -> t option
    val get : path -> t -> t option
    val set_offset : offset -> t -> t -> t option
    val set : path -> t -> t -> t option
  end
