val ( % ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
module MyList :
  sig
    val is_empty : 'a list -> bool
    val length : 'a list -> int
    val sum : int list -> int
    val avg : int list -> float option
    val sumf : float list -> float
    val avgf : float list -> float option
    val append : 'a list -> 'a list -> 'a list
    val ( @ ) : 'a list -> 'a list -> 'a list
    val palindrome : 'a list -> 'a list
    val head : 'a list -> 'a option
    val tail : 'a list -> 'a list option
    val last : 'a list -> 'a option
    val reverse : 'a list -> 'a list
    val is_palindrome : 'a list -> bool
    val map : ('a -> 'b) -> 'a list -> 'b list
    val double : int list -> int list
    val filter : ('a -> bool) -> 'a list -> 'a list
    val even : int -> bool
    val neg : ('a -> bool) -> 'a -> bool
    val odd : int -> bool
    val only_even : int list -> int list
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
    val exists : ('a -> bool) -> 'a list -> bool
    val for_all : ('a -> bool) -> 'a list -> bool
  end
module NonEmptyList :
  sig
    type 'a t = Cons of 'a * 'a t | Nil of 'a
    val from_list : 'a list -> 'a t option
    val to_list : 'a t -> 'a list
    val head : 'a t -> 'a
    val tail : 'a t -> 'a t option
    val last : 'a t -> 'a
  end
module Db :
  sig
    type student = { sname : string; age : int; semester : int; }
    type course = { cname : string; lecturer : string; }
    type grade = { student : string; course : string; grade : float; }
    val students : student list
    val courses : course list
    val grades : grade list
    val find_student : string -> student option
    val avg_age : int -> float option
    val avg_grade_student : string -> float option
    val avg_grade_course : string -> float option
    val avg_grade_course_passed : string -> float option
    val avg_grade_course_semester : string -> int -> float option
  end
