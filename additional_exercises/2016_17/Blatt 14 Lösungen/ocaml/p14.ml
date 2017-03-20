
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

let multiply vector matrix =
  (* miso: master in / slave out, mosi: master out, slave in *)
  let transpose_miso = new_channel () in
  (* different types require different channel *)
  let (mul_miso, mul_mosi) = (new_channel (), new_channel ()) in
  let (sum_miso, sum_mosi) = (new_channel (), new_channel ()) in
  (* Todo *)

let () =
  let matrix = [
    [1; 2; 3];
    [4; 5; 6]
  ] in
  let vector = [2; 3] in
  ()
