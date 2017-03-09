(* generate .mli with `ocamlc -i` *)

let (%) g f x = g (f x)

module MyList = struct
  (*
   * #show_type list;;
   * type nonrec 'a list = [] | :: of 'a * 'a list
   *)
  let is_empty xs = xs = []
  let rec length = function [] -> 0 | _::xs -> 1 + length xs
  let rec sum = function [] -> 0 | x::xs -> x + sum xs

  let avg = function [] -> None | xs -> Some (float_of_int (sum xs) /. float_of_int (length xs))
  let rec sumf = function [] -> 0. | x::xs -> x +. sumf xs
  let avgf = function [] -> None | xs -> Some (sumf xs /. float_of_int (length xs))

  let rec append a b = (* list concatenation, same as @ (will be undefined in test env!) *)
    match a with
    | [] -> b
    | x::xs -> x :: append xs b
  (* but we want some infix operators!
   * https://realworldocaml.org/v1/en/html/variables-and-functions.html#table2_1
   *)
  let (@) = append
  let rec palindrome = function [] -> [] | x::xs -> x :: palindrome xs @ [x]
  (* exceptions are evil! *)
  (*
   * #show_type option;;
   * type nonrec 'a option = None | Some of 'a
   *)
  let head = function [] -> None | x::_ -> Some x
  let tail = function [] -> None | _::xs -> Some xs
  let rec last = function [] -> None | [x] -> Some x | _::xs -> last xs
  let rec reverse = function [] -> [] | x::xs -> reverse xs @ [x]
  let is_palindrome xs = xs = reverse xs
  let rec map f = function [] -> [] | x::xs -> f x :: map f xs
  let double = map (( * ) 2)
  let rec filter p = function [] -> [] | x::xs -> if p x then x :: filter p xs else filter p xs
  let even x = x mod 2 = 0
  let neg f x = not (f x)
  let odd = neg even
  let only_even = filter even
  let rec fold_left f a = function [] -> a | x::xs -> fold_left f (f a x) xs
  let rec fold_right f = function [] -> (fun x -> x) | x::xs -> fun a -> f x (fold_right f xs a) (* not tail recursive! *)
  let exists p xs = fold_right ((||)%p) xs false
  let for_all p xs = fold_right ((&&)%p) xs true
end

module NonEmptyList = struct
  type 'a t = Cons of 'a * 'a t | Nil of 'a

  (* let (|?) x y = match x with Some z -> z | None -> y *)
  let rec from_list = function
    | [] -> None
    | x::xs ->
      Some (match from_list xs with
      | None -> Nil x
      | Some z -> Cons (x, z))
  let rec to_list = function
    | Cons (x,xs) -> x :: to_list xs
    | Nil x -> [x]

  let head = function Cons (x,_) | Nil x -> x
  let tail = function Cons (_,xs) -> Some xs | _ -> None
  let rec last = function Nil x -> x | Cons (_,xs) -> last xs
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
  let find_student name = head (filter (fun x -> x.sname = name) students)
  (* all averages are of type float option *)
  (* calculate the average age of students that are in a given semester or above *)
  let avg_age sem = filter (fun x -> x.semester >= sem) students |> map (fun x -> x.age) |> avg
  let avg_grade p = filter p grades |> map (fun x -> x.grade) |> avgf
  (* calculate the grade average of a student *)
  let avg_grade_student name = avg_grade (fun x -> x.student = name)
  (* calculate the grade average of a course *)
  let avg_grade_course name = avg_grade (fun x -> x.course = name)
  (* calculate the grade average of a course (only passed, i.e. grade <= 4.0) *)
  let avg_grade_course_passed name = avg_grade (fun x -> x.course = name && x.grade <= 4.0)
  (* calculate the grade average of a course for students in a given semester *)
  let avg_grade_course_semester name sem = avg_grade (fun x -> x.course = name && match find_student x.student with Some s -> s.semester = sem | _ -> false)
end
