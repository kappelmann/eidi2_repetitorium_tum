(* generate .mli with `ocamlc -i` *)

exception Invalid_input

let hello name = "Hello "^name^"!"

let rec fac n =
  if n<0 then raise Invalid_input
  else if n<=1 then n
  else n * fac (n-1)

let rec fib n =
  if n<0 then raise Invalid_input
  else if n<=1 then n
  else fib (n-1) + fib (n-2)

let rec even_sum n =
  if n<0 then raise Invalid_input
  else if n=0 then 0
  else (if n mod 2 = 0 then n else 0) + even_sum (n-1);;

let is_empty xs = xs = []

let rec length = function [] -> 0 | _::xs -> 1 + length xs

let rec sum = function [] -> 0 | x::xs -> x + sum xs
