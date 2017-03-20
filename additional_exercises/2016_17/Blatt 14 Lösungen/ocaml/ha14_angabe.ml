open Sexplib.Std
open Unix

type user = {
  first_name : string;
  second_name : string
} [@@deriving sexp]

type seat = int
[@@deriving sexp]

type request =
    Register of user
  | Status of user
  | Deregister of user
[@@deriving sexp]

type response =
    Success of seat
  | DeregisterSuccess
  | InvalidRequest
  | UnknownUser
  | AlreadyRegistered
  | NoMoreTickets
[@@deriving sexp]

let bind_listen bind_addr port =
  let serveraddr = ADDR_INET (bind_addr, port) in
  let sock = Unix.socket PF_INET Unix.SOCK_STREAM 0 
  in Unix.bind sock serveraddr;
  Unix.listen sock 3;
  sock