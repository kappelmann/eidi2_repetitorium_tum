open Ha14_angabe

open Sys
open Unix
open Thread
open Event

(* Modul für einen Duplex-Channel mit unterschiedlichen Message-Typen in
   jede Richtung *)
module Duplex = struct
  type ('a, 'b) t = { mosi : 'a Event.channel; miso : 'b Event.channel }

  let create () = { mosi = new_channel (); miso = new_channel () }

  let master_ssend chan data = sync (send chan.mosi data)
  let slave_ssend chan data = sync (send chan.miso data)
  let master_sreceive chan = sync (receive chan.miso)
  let master_receive chan = receive chan.miso
  let slave_sreceive chan = sync (receive chan.mosi)
end

(* Modul zum Management von Tickets; verwaltet je eine Liste freier und
   bereits vergebener Tickets *)
module TicketManager = struct
  type t = { free_tickets : seat list; allocations : (user * seat) list }

  let create n = {
    free_tickets = Batteries.List.init n (fun x -> x);
    allocations = []
  }

  let allocate s user = try
      let _ = List.assoc user s.allocations in
      (s, AlreadyRegistered) with
    Not_found ->
    match s.free_tickets with
      [] -> (s, NoMoreTickets)
    | seat::xs ->
      let s' = { free_tickets = xs;  allocations = (user, seat)::s.allocations} in
      (s', Success seat)

  let status s user = try
      let seat = List.assoc user s.allocations in
      Success seat with
    Not_found -> UnknownUser

  let deallocate s user = try
      let ticket = List.assoc user s.allocations in
      let alloc' = List.remove_assoc user s.allocations in
      let s' = { free_tickets = ticket::s.free_tickets;  allocations = alloc'} in
      (s', DeregisterSuccess) with
    Not_found -> (s, UnknownUser)
end

(* Thread für je einen Client *)
let handle_client (sock, worker_channel) =
  let inchan = Unix.in_channel_of_descr sock
  and outchan = Unix.out_channel_of_descr sock in
  (* Warten auf Request von Client *)
  let request_line = input_line inchan in
  let response = try
      let request = request_of_sexp (Sexplib.Sexp.of_string request_line) in
      (* Senden des Requests an den Master *)
      Duplex.slave_ssend worker_channel request;
      (* Warten auf Antwort von Master *)
      Duplex.slave_sreceive worker_channel
    with _ -> InvalidRequest in
  (* Senden der Antwort an den Client *)
  let response_line = Sexplib.Sexp.to_string (sexp_of_response response) in
  Printf.fprintf outchan "%s\n" response_line;
  flush outchan;
  Unix.close sock

(* Thread für den Server; wartet unentwegt auf neue Verbindungen *)
let server (sock, worker_channel) =
  while true do
    let (s, _) = Unix.accept sock  in
    let _ = create handle_client (s, worker_channel) in ()
  done

(* Master-Thread *)
let master bind_addr port tickets =
  let worker_channel = Duplex.create () in
  let server_sock = bind_listen bind_addr port in
  let _ = create server (server_sock, worker_channel) in
  (* Nachrichten-Schleife, die auf Requests reagiert *)
  let rec loop state =
    let handle_request request =
      let (s', response) = match request with
          Register user -> TicketManager.allocate state user
        | Status user -> (state, TicketManager.status state user)
        | Deregister user -> TicketManager.deallocate state user
      in Duplex.master_ssend worker_channel response; s' in
    (* Auf Request von Client-Thread warten *)
    let request = Duplex.master_sreceive worker_channel in
    (* Antwort erzeugen und zurückschicken *)
    let state' = handle_request request in
    loop state'
  in loop (TicketManager.create tickets)

(*let () =
  let foo = Register {first_name = "Hans"; second_name = "Peter"} in
  Printf.printf "%s\n" (Sexplib.Sexp.to_string (sexp_of_request foo));
  Pervasives.flush Pervasives.stdout;
  master Unix.inet_addr_any 4445 3*)
