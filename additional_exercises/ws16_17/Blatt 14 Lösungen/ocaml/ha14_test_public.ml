open Kaputt.Abbreviations
open Ha14_angabe
open Batteries.List

open Sys
open Unix
open Thread
open Event

module X = Ha14

let port = ref 0

let tests = ref []
let points = ref []
let add points_test test =
  points := !points @ [points_test];
  tests := !tests @ [test]

let start_master n =
  let port' = !port in
  port := port' + 1;
  let _ = create (fun () -> X.master Unix.inet_addr_loopback port' n) () in
  delay 0.1;
  port'

let connect port' =
  let sock = socket PF_INET SOCK_STREAM 0 in
  connect sock (ADDR_INET(Unix.inet_addr_loopback, port'));
  let inchan = Unix.in_channel_of_descr sock
  and outchan = Unix.out_channel_of_descr sock in
  (sock, inchan, outchan)

let req (inchan, outchan) request =
  let request_line = Sexplib.Sexp.to_string (sexp_of_request request) in
  Printf.fprintf outchan "%s\n" request_line;
  Pervasives.flush outchan

let res (inchan, outchan) =
  let response_line = input_line inchan in
  response_of_sexp (Sexplib.Sexp.of_string response_line)

let close sock =
  Unix.shutdown sock Unix.SHUTDOWN_ALL;
  Unix.close sock

let communicate port request =
  let (sock, inchan, outchan) = connect port in
  req (inchan, outchan) request;
  let r = res (inchan, outchan) in
  close sock;
  r

let communicate_inv port =
  let (sock, inchan, outchan) = connect port in
  Printf.fprintf outchan "Naaaa, wie parst sich das?\n";
  Pervasives.flush outchan;
  let r = res (inchan, outchan) in
  close sock;
  r

let success response =
  match response with
    Success v -> v
  | _ -> Assert.fail_msg "Failed!"

let test_simple () =
  let port = start_master 10 in
  let _ = success (communicate port (Register { first_name = "Atom"; second_name = "Fried" })) in
  ()

let test_multi_register_status_sync () =
  let port = start_master 6 in
  let people = [
    { first_name = "Hans"; second_name = "Peter" };
    { first_name = "Martina"; second_name = "Atom" };
    { first_name = "Inge"; second_name = "Hubertus" };
    { first_name = "Frinz"; second_name = "Fronz" };
    { first_name = "Tins"; second_name = "Tonz" };
    { first_name = "Gundula"; second_name = "Dado" };
  ] in
  let tickets = List.map (fun person ->
      success (communicate port (Register person))) people in 
  List.iter2 (fun person ticket ->
      Assert.equal ticket (success (communicate port (Status person)))) people tickets

let rec destutter = function
    x::y::xs -> if x = y then destutter xs else x :: destutter (y::xs)
  | l -> l

let test_multi_all_sync () =
  let port = start_master 6 in
  let communicate = communicate port in
  let people = [
    ({ first_name = "Hans"; second_name = "Flunz" }, true);
    ({ first_name = "Martina"; second_name = "Atom" }, false);
    ({ first_name = "Pamela"; second_name = "Foobar" }, false);
    ({ first_name = "Grunzgunde"; second_name = "Hase" }, true);
    ({ first_name = "Atomfried"; second_name = "Atom" }, false);
    ({ first_name = "Inge"; second_name = "Hubertus" }, true);
  ] in
  let tickets = List.map (fun (person, _) ->
      success (communicate (Register person))) people in
  List.iter (fun (person, dereg) ->
      if dereg then Assert.equal DeregisterSuccess (communicate (Deregister person))) people;
  Assert.equal InvalidRequest (communicate_inv port);
  List.iter (fun (person, dereg) ->
      if dereg then Assert.equal UnknownUser (communicate (Deregister person))
      else Assert.equal AlreadyRegistered (communicate (Register person))) people;
  let tickets = List.map2 (fun (person, dereg) ticket ->
      if dereg then success (communicate (Register person)) else ticket) people tickets in
  let tickets' = destutter (sort Pervasives.compare tickets) in
  (*Printf.printf "%s\n" ([%derive.show: int list] tickets); Pervasives.flush Pervasives.stdout;*)
  Assert.equal InvalidRequest (communicate_inv port);
  Assert.equal (List.length tickets) (List.length tickets');
  Assert.equal NoMoreTickets (communicate (Register { first_name = "Dagobert"; second_name = "Walter" }));
  List.iter2 (fun (person, _) ticket ->
      Assert.equal ticket (success (communicate (Status person)))) people tickets;
  Assert.equal NoMoreTickets (communicate (Register { first_name = "Hugo"; second_name = "Hubertus" }))

let test_multi_all_async () =
  let port = start_master 3 in
  let communicate = communicate port in
  let p1 = { first_name = "Hans"; second_name = "Franz" } in
  let p2 = { first_name = "Peter"; second_name = "Lustig" } in
  let p3 = { first_name = "Inge"; second_name = "Egni" } in
  let p4 = { first_name = "Fluse"; second_name = "Flasi" } in
  let repack (a, b, c) = (a, (b, c)) in
  let (sock1, conn1) = repack (connect port) in
  let (sock2, conn2) = repack (connect port) in
  req conn1 (Register p1);
  let (sock3, conn3) = repack (connect port) in
  let t1 = success (res conn1) in
  req conn3 (Register p3);
  let t4 = success (communicate (Register p4)) in
  req conn2 (Register p2);
  Assert.equal NoMoreTickets (res conn2);
  close sock2;
  let (sock5, conn5) = repack (connect port) in
  close sock1;
  let (sock6, conn6) = repack (connect port) in
  let t3 = success (res conn3) in
  req conn6 (Status p4);
  req conn5 (Status p1);
  close sock3;
  let t1' = success (res conn5) in
  Assert.equal t1 t1';
  let t4' = success (res conn6) in
  Assert.equal t4 t4';
  let t3' = success (communicate (Status p3)) in
  Assert.equal t3 t3';
  close sock5;
  close sock6

let () =
  add 0 @@ Test.make_simple_test ~title:"test_simple" test_simple;
  add 0 @@ Test.make_simple_test ~title:"test_multi_register_status_sync" test_multi_register_status_sync;
  add 0 @@ Test.make_simple_test ~title:"test_multi_all_sync" test_multi_all_sync;
  add 0 @@ Test.make_simple_test ~title:"test_multi_all_async" test_multi_all_async

(* launch *)
let () =
  Random.self_init ();
  port := (Random.int 60000) + 2000;
  let point = Test.(function
      | Passed -> 1
      | Report (p,n,e,c,m) when p = n -> 1
      | _ -> 0)
  in
  let passed = List.map point (Test.exec_tests !tests) in
  Test.run_tests ~output:(Test.Html_output (open_out "result.html")) !tests;
  Test.run_tests !tests;
  prerr_endline @@ "### GRADES: " ^ String.concat " "
  @@ List.map2 (fun x y -> string_of_int (y*x)) passed !points
