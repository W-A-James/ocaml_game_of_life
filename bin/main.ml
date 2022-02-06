open Ocaml_game_of_life

let () = (
    let window = Gui.new_game_window 800 800 50 50 in
    try
        while true do
            Gui.draw_board window;
            window.game_board <- Game_of_life.update_board window.game_board;
            Unix.sleepf 0.1
        done
    with Exit -> ()
)
