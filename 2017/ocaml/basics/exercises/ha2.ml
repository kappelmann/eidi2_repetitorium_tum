let todo _ = failwith "TODO"
let (%) g f x = g (f x)

module MyList = struct
  (* you can have a look at types in the REPL: *)
  (*
   * #show_type list;;
   * type nonrec 'a list = [] | :: of 'a * 'a list
   *)
  let is_empty xs = xs = []
  let rec length = function [] -> 0 | _::xs -> 1 + length xs
  let rec sum = function [] -> 0 | x::xs -> x + sum xs

  (* average of an int list as float *)
  let avg = todo
  (* and for float list: *)
  let sumf = todo
  let avgf = todo

  (* list concatenation, same as @ (will be undefined in test env!) *)
  let append a = todo
  let (@) = append (* we want some infix operators! *)
  let palindrome = todo (* e.g. palindrome ['a','b'] = ['a','b','b','a'] *)

  (* exceptions are evil! *)
  (*
   * #show_type option;;
   * type nonrec 'a option = None | Some of 'a
   *)
  let head = todo
  let tail = todo
  let last = todo

  let reverse = todo
  let is_palindrome = todo
  let map f = todo
  let double = todo
  let filter p = todo
  let even = todo
  let neg = todo (* negate a function *)
  let odd = todo
  let only_even = todo
  let fold_left f a = todo
  let fold_right f xs = todo
  let exists p = todo
  let for_all p = todo
end

module NonEmptyList = struct
  type 'a t = Cons of 'a * 'a t | Nil of 'a

  let from_list = todo
  let to_list = todo

  let head = todo
  let tail = todo
  let last = todo
end

module Db = struct
  (* we assume that name is unique here *)
  type student = { sname : string; age : int; semester: int }
  type course = { cname : string; lecturer : string }
  type grade = { student : string; course : string; grade : float }

  let students = [
    { sname = "Student 1"; age = 19; semester = 1 };
    { sname = "Student 2"; age = 24; semester = 7 };
    { sname = "Student 3"; age = 28; semester = 12 };
    { sname = "Student 4"; age = 23; semester = 4 };
  ]
  let courses = [
    { cname = "Course 1"; lecturer = "Prof. 1" };
    { cname = "Course 2"; lecturer = "Prof. 2" };
    { cname = "Course 3"; lecturer = "Prof. 1" };
  ]
  let grades = [
    { student = "Student 1"; course = "Course 1"; grade = 2.7 };
    { student = "Student 1"; course = "Course 2"; grade = 1.0 };
    { student = "Student 2"; course = "Course 1"; grade = 4.0 };
    { student = "Student 2"; course = "Course 2"; grade = 5.0 };
    { student = "Student 3"; course = "Course 3"; grade = 3.7 };
  ]

  open MyList
  (* find a student by name *)
  let find_student = todo
  (* all averages are of type float option *)
  (* calculate the average age of students that are in a given semester or above *)
  let avg_age = todo
  (* calculate the grade average of a student *)
  let avg_grade_student = todo
  (* calculate the grade average of a course *)
  let avg_grade_course = todo
  (* calculate the grade average of a course (only passed, i.e. grade <= 4.0) *)
  let avg_grade_course_passed = todo
  (* calculate the grade average of a course for students in a given semester *)
  let avg_grade_course_semester name = todo
end
