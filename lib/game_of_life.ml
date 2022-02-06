type cell = Alive | Dead

let cell_to_string c = match c with Alive -> "#" | Dead -> " "

type board = { width : int; height : int; cells : cell list }

let new_board w h =
  { width = w; height = h; cells = List.init (w * h) (fun _ -> Dead) }

let display_board b =
  List.iteri
    (fun (index : int) (c : cell) ->
      if Int.rem index b.width = 0 then Printf.printf "\n%s" (cell_to_string c)
      else Printf.printf "%s" (cell_to_string c))
    b.cells;
  print_endline ""

let new_random_board w h =
  Random.self_init ();
  {
    width = w;
    height = h;
    cells = List.init (w * h) (fun _ -> if Random.bool () then Alive else Dead);
  }

let get_cell game_board i j =
  List.nth game_board.cells ((j * game_board.width) + i)

let get_num_living_neighbours (game_board : board) (i : int) (j : int) : int =
  let num_neighbours = ref 0 in
  let prev_j = if j = 0 then game_board.height - 1 else j - 1 in
  let next_j = Int.rem (j + 1) game_board.height in
  let prev_i = if i = 0 then game_board.width - 1 else i - 1 in
  let next_i = Int.rem (i + 1) game_board.width in

  let check_cell x y =
    if get_cell game_board x y = Alive then
      num_neighbours := !num_neighbours + 1
  in
  (* Line above (j - 1) *)
  check_cell prev_i prev_j;
  check_cell i prev_j;
  check_cell next_i prev_j;

  (* Left and right neighbours (j) *)
  check_cell prev_i j;
  check_cell next_i j;

  (* Line below (j + 1) *)
  check_cell prev_i next_j;
  check_cell i next_j;
  check_cell next_i next_j;

  !num_neighbours

let get_neighbour_list (game_board : board) : int list =
    (**)
  let index = ref ((game_board.height * game_board.width) - 1) in
  List.rev (List.rev_map
    (fun _ ->
      let n =
        get_num_living_neighbours game_board
          (Int.rem !index game_board.width)
          (Int.div !index game_board.width)
      in
      index := !index - 1;
      n)
    game_board.cells)

(* TODO: Return list of diffs and operate on this instead *)
let update_board b =
  let neighbour_list = get_neighbour_list b in
  let new_cells =
    List.rev_map2
      (fun n c ->
        match c with
        | Alive -> if n < 2 || n > 3 then Dead else Alive
        | Dead -> if n = 3 then Alive else Dead)
      neighbour_list b.cells
  in
  { width = b.width; height = b.height; cells = new_cells }

let rec loop i loop_fn end_fn =
  match i with
  | 0 -> end_fn ()
  | n ->
      loop_fn ();
      loop (n - 1) loop_fn end_fn
