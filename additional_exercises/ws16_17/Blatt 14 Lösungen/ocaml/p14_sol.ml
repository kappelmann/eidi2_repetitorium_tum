
open Sys
open Unix
open Thread
open Event

type col = int list
[@@deriving show]

type matrix = col list
[@@deriving show]

(* transposer *)
let transpose (miso, matrix) =
  let rec foreach_column rest =
    (* transpose *)
    let (column, rest) = List.fold_right (fun x (c, r) ->
        match x with
          hd::tl -> (hd::c, tl::r)
        | _ -> ([], [])
      ) rest ([], []) in
    (* send result *)
    sync (send miso (Some column));
    (* continue if not finished *)
    match rest with
      hd::tl -> (match hd with
          [] -> ()
        | _ -> foreach_column rest)
    | _ -> assert false in
  foreach_column matrix;
  sync (send miso None)

(* multiplier *)
let multiply (v, miso, mosi) =
  let rec inner () =
    (* receive transposed line *)
    let column = (sync (receive mosi)) in
    match column with
      Some column ->
      (* multiply *)
      let multiplied = List.map2 (fun x y -> x*y) v column in
      (* send result to master *)
      sync (send miso multiplied);
      (* continue *)
      inner ()
    (* Got a None? Finished! *)
    | _ -> () in
  inner ()

(* summer *)
let sum (miso, mosi) =
  let rec inner () =
    (* receive transposed and multiplied line *)
    let vec = (sync (receive mosi)) in
    match vec with
      Some vec ->
      (* sum *)
      let sum = List.fold_left (+) 0 vec in
      (* send result to master *)
      sync (send miso sum);
      (* continue *)
      inner ()
    (* Got a None? Finished! *)
    | _ -> () in
  inner ()

type worker =
    Multiplier
  | Summer

type state = {
  (* already transposed lines *)
  trd : int list list;
  (* already multiplied lines *)
  multd : int list list;
  (* result numbers *)
  result : int list;
  (* free workers *)
  free : worker list;
  (* Is the transposer finished? *)
  trans_finished : bool
}

let start_state = {
  trd = [];
  multd = [];
  result = [];
  free = [Multiplier; Summer];
  trans_finished = false
}

let multiply vector matrix =
  (* miso: master in / slave out, mosi: master out, slave in *)
  let transpose_miso = new_channel () in
  (* different types require different channel *)
  let (mul_miso, mul_mosi) = (new_channel (), new_channel ()) in
  let (sum_miso, sum_mosi) = (new_channel (), new_channel ()) in
  let transposer = create transpose (transpose_miso, matrix) in
  let multiplier = create multiply (vector, mul_miso, mul_mosi) in
  let summer = create sum (sum_miso, sum_mosi) in
  let rec loop (state : state) =
    (* assign work if there is work and a free worker *)
    let state = List.fold_left (fun s worker ->
        match (worker, state) with
          (Multiplier, {trd = thd::ttl}) -> sync (send mul_mosi (Some thd));
          {s with trd = ttl}
        | (Summer, {multd = mulhd::multl}) -> sync (send sum_mosi (Some mulhd));
          {s with multd = multl}
        | _ -> {s with free = worker::s.free}
      ) {state with free = []} state.free in
    (* If the transposer is finished and there is no work for the other workers,
       all work is done. *)
    if state.trans_finished && (List.length state.free) == 2 then state else
      (* Either of the three workers will send a message. The message is accumulated.
         In case the worker is also a receiver (Multiplier, Summer), we add the worker
         to the free list so that it gets new work assigned. *)
      loop (select [
          wrap (receive transpose_miso) (function
                None -> {state with trans_finished = true}
              | Some col -> {state with trd = state.trd @ [col]});
          wrap (receive mul_miso) (fun vec ->
              {state with multd = state.multd @ [vec]; free = Multiplier::state.free});
          wrap (receive sum_miso) (fun v ->
              {state with result = state.result @ [v]; free = Summer::state.free});
        ])
  in let state = loop start_state in
  (* clean up; terminate workers *)
  sync (send mul_mosi None);
  sync (send sum_mosi None);
  (* workers should terminate; wait for them *)
  join transposer;
  join multiplier;
  join summer;
  state.result

let () =
  let matrix = [
    [1; 2; 3];
    [4; 5; 6]
  ] in
  let vector = [2; 3] in
  let result = multiply vector matrix in
  Printf.printf "%s\n" (show_col result)
