open Game_of_life

type game_window = {
  width : int;
  height : int;
  mutable game_board : board;
  cell_width : int;
  cell_height : int;
  mutable current_img : Graphics.image;
}

let new_game_window w_px h_px game_width game_height =
  let open Graphics in
  open_graph (Printf.sprintf " %dx%d" w_px h_px);
  clear_graph ();
  auto_synchronize false;
  remember_mode true;
  display_mode true;
  let board = new_random_board game_width game_height in
  let init_diff = get_diffs board in
  ( {
      width = w_px;
      height = h_px;
      game_board = board;
      cell_width = Int.div w_px game_width + Int.rem w_px game_width;
      cell_height = Int.div h_px game_height + Int.rem h_px game_height;
      current_img = get_image 0 0 w_px h_px;
    },
    init_diff )

let draw_board (b : game_window) (d : diff list) =
  let open Graphics in
  clear_graph ();
  draw_image b.current_img 0 0;
  (* Draw board *)
  List.iter
    (fun d ->
      match d with
      | Diff (ind, change) ->
          let colour = match change with Alive -> black | Dead -> white in
          let col_num = Int.rem ind b.game_board.width in
          let row_num =
            b.game_board.height - 1 - Int.div ind b.game_board.height
          in
          let left = col_num * b.cell_width in
          let bottom = row_num * b.cell_height in
          set_color colour;
          fill_rect left bottom b.cell_width b.cell_height)
    d;
    blit_image b.current_img 0 0;
  synchronize ()
