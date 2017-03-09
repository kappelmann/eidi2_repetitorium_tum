open Ha9_angabe

(* Hilfsfunktion, die verwendet werden kann, um eine Datei nach Benutzung
   immer sicher zu schließen *)
let protect ~f ~finally = try
    let result = f () in finally (); result with
  ex -> finally (); raise ex

let read_csv (init : 'a) (setters : 'a setter list) (file_name : string) =
  let setters_length = List.length setters in
  let rows = ref [] in
  let process_line line =
    let splitted = Str.split (Str.regexp ";") line in
    if (List.length splitted) != setters_length then
      raise Column_count_does_not_match;
    let process_column acc column setter =
      setter column acc in
    (* Der jeweilige Setter wird auf seine Spalte angewendet. *)
    let row = List.fold_left2 process_column init splitted setters in
    (* Das Ergebnis ist eine neue Zeile. *)
    rows := row :: !rows; in
  let file = open_in file_name in
  protect ~f:(fun () ->
      let rec read_file () = try
          let line = input_line file in
          process_line line;
          read_file ();
        with End_of_file -> () in
      read_file ()
    ) ~finally:(fun () -> close_in file);
  (* Das wir neue Zeilen immer vorne anhängen, ist die Reihenfolge
     der Zeilen verdreht. *)
  List.rev !rows

let group_by csv getter =
  let process_row groups row =
    let key = getter row in try
      let key_rows = List.assoc key groups in
      (key, row :: key_rows) :: (List.remove_assoc key groups)
    with Not_found -> (key, [row]) :: groups in
  (*
    List.fold_left: Einfügen der Elemente in eine Association List
    List.split: Liste von Tupeln -> Tupel von Listen (Keys und Values)
    snd: Werte herausnehmen
  *)
  snd (List.split (List.fold_left process_row [] csv))

type tree_decoration = { layer : int; pos : int; decoration : char }

let write_xmas file_name decor_file_name height =
  (* Schmuckstücke einlesen *)
  let decor_csv = read_csv { layer = 0; pos = 0; decoration = '0'} [
      (fun v curr -> {curr with layer = int_of_string v});
      (fun v curr -> {curr with pos = int_of_string v});
      (fun v curr -> {curr with decoration = if String.length v != 1 then
                                    raise (Invalid_decoration v) else
                                    String.get v 0 });
    ] decor_file_name in 
  let compare_decorations a b = if a.layer < b.layer then (-1)
    else if a.layer > b.layer then 1
    else if a.pos < b.pos then (-1)
    else if a.pos > b.pos then 1
    else 0 in
  (* Schmuckstücke sortieren *)
  let decorations_sorted = ref (List.sort compare_decorations decor_csv) in
  (* Der Baum ist ganz unten am breitesten; alle Zeilen haben die entsprechende
     Länge. *)
  let line_length = 2*(height - 1) + 1 in
  let build_line layer = 
    let spaces_side = (line_length - 1 - 2*layer)/2 in
    (* Die Schmuckstücke wurden oben sortiert und können daher der Reihe nach
       herausgenommen werden. *)
    let inner_tree_decorated i = match !decorations_sorted with
        hd :: tl when hd.layer = layer && hd.pos = i ->
        decorations_sorted := tl;
        String.make 1 hd.decoration
      | _ -> "*" in
    (* Eine Zeile besteht aus zwei Auffüllungen (rechts und links)
       und dem Baum. *)
    (Batteries.List.make spaces_side " ") @
    (Batteries.List.init (line_length - 2*spaces_side) inner_tree_decorated) @
    (Batteries.List.make spaces_side " ") in
  let file = open_out file_name in
  protect ~f:(fun () -> 
      (* Zunächst wird der Baum ausgegeben. *)
      for i = 0 to (height - 1) do
        Printf.fprintf file "%s\n" (String.concat "" (build_line i))
      done;
      (* Es folgt der Stamm. *)
      let trunc_dist = (line_length - 3)/2 in
      let trunc_line = String.concat "" (
          (Batteries.List.make trunc_dist " ") @
          (Batteries.List.make 3 "*") @
          (Batteries.List.make trunc_dist " ")) in
      for i = 0 to height/3 do
        Printf.fprintf file "%s\n" trunc_line
      done;
    ) ~finally:(fun () -> close_out file);
  if !decorations_sorted != [] then
    raise Could_not_decorate
