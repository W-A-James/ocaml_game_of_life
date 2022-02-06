type cell = Alive | Dead
type diff = Diff of int * cell

let cell_to_string c = match c with Alive -> "#" | Dead -> " "

type board = {
  width : int;
  height : int;
  cells : cell array;
  neighbours : int array;
}

let new_board (w : int) (h : int) =
  {
    width = w;
    height = h;
    cells = Array.make (w * h) Dead;
    neighbours = Array.make (w * h) 0;
  }

let display_board (b : board) =
  Array.iteri
    (fun (index : int) (c : cell) ->
      if Int.rem index b.width = 0 then Printf.printf "\n%s" (cell_to_string c)
      else Printf.printf "%s" (cell_to_string c))
    b.cells;
  print_endline ""

let get_cell game_board i j = game_board.cells.((j * game_board.width) + i)

let get_cell_neighbours (game_board : board) (i : int) (j : int) : int =
  let prev_j = if j = 0 then game_board.height - 1 else j - 1 in
  let next_j = Int.rem (j + 1) game_board.height in
  let prev_i = if i = 0 then game_board.width - 1 else i - 1 in
  let next_i = Int.rem (i + 1) game_board.width in

  let pairs =
    [
      (prev_i, prev_j);
      (i, prev_j);
      (next_i, prev_j);
      (prev_i, j);
      (next_i, j);
      (prev_i, next_j);
      (next_i, next_j);
      (i, next_j);
    ]
  in

  List.fold_left
    (fun acc p ->
      let x, y = p in
      if get_cell game_board x y = Alive then acc + 1 else acc)
    0 pairs

let update_neigbours (b : board) =
  Array.iteri
    (fun i _ ->
      let x = Int.rem i b.width in
      let y = Int.div i b.width in
      let n = get_cell_neighbours b x y in
      b.neighbours.(i) <- n)
    b.cells

let new_random_board w h =
  Random.self_init ();
  let cells = Array.make (w * h) Dead in
  Array.iteri
    (fun i _ -> cells.(i) <- (if Random.bool () then Alive else Dead))
    cells;
  let b = { width = w; height = h; cells; neighbours = Array.make (w * h) 0 } in
  update_neigbours b;
  b

let get_diffs (b : board) : diff list =
  let ind = ref 0 in
  Array.fold_left
    (fun acc c ->
      let neighbours = b.neighbours.(!ind) in
      let new_diff =
        match c with
        | Alive ->
            if neighbours < 2 || neighbours > 3 then [ Diff (!ind, Dead) ]
            else []
        | Dead -> if neighbours = 3 then [ Diff (!ind, Alive) ] else []
      in
      ind := !ind + 1;
      List.rev_append new_diff acc)
    [] b.cells

let update_board (b : board) : diff list =
  update_neigbours b;
  let diffs = get_diffs b in
  List.iter
    (fun x -> match x with Diff (i, change) -> b.cells.(i) <- change)
    diffs;
  diffs

let randomize (b : board) =
  Array.iteri
    (fun i _ -> b.cells.(i) <- (if Random.bool () then Alive else Dead))
    b.cells;
  update_neigbours b
