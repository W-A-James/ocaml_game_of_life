open Game_of_life

type game_window = {
  width : int;
  height : int;
  mutable game_board : board;
  cell_width : int;
  cell_height : int;
}

let new_game_window w_px h_px game_width game_height =
  let open Graphics in
  open_graph (Printf.sprintf " %dx%d" w_px h_px);
  auto_synchronize false;
  {
    width = w_px;
    height = h_px;
    game_board = new_random_board game_width game_height;
    cell_width = Int.div w_px game_width + Int.rem w_px game_width;
    cell_height = Int.div h_px game_height + Int.rem h_px game_height;
  }

let draw_board (b : game_window) =
  let open Graphics in
  clear_graph();
  remember_mode true;
  (* Draw board *)
  List.iteri
    (fun i cell ->
      let colour = match cell with Alive -> black | Dead -> white in
      let col_num = Int.rem i b.game_board.width in
      let row_num = b.game_board.height - 1 - Int.div i b.game_board.height in
      let left = col_num * b.cell_width in
      let bottom = row_num * b.cell_height in
      set_color colour;
      fill_rect left bottom b.cell_width b.cell_height)
    b.game_board.cells;
  remember_mode false;
  synchronize ()
