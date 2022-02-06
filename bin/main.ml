open Ocaml_game_of_life

let usage = ""
let width = ref 100
let height = ref 100
let refresh_period = ref 0.05
let debug = ref false

let speclist =
  [
    ("-w", Arg.Set_int width, "Game Width");
    ("-h", Arg.Set_int height, "Game Height");
    ("-r", Arg.Set_float refresh_period, "Time between frames");
  ]

let () =
  let debug_print s = if !debug then print_endline s in

  let diffs = ref [] in
  let window, diff = Gui.new_game_window 800 600 !width !height in
  diffs := diff;
  let m = Mutex.create () in
  let ready_for_update = Condition.create () in
  let refresh = ref false in

  let drawing_loop () =
    try
      while true do
        (if Graphics.key_pressed () then
         let current_key = Graphics.read_key () in
         match current_key with
         | 'q' -> exit 0
         | 'r' ->
             debug_print "Refreshing";
             refresh := true;
             Mutex.lock m;

             Graphics.clear_graph ();
             Graphics.set_color Graphics.background;
             Graphics.draw_rect 0 0 window.width window.height;
             Graphics.synchronize ();
             Graphics.blit_image window.current_img 0 0;
             Graphics.synchronize ();

             Condition.signal ready_for_update;
             Mutex.unlock m;

             Thread.yield ()
         | _ -> ());

        debug_print "main waiting to draw";
        Mutex.lock m;
        debug_print "main drawing";

        Gui.draw_board window !diffs;
        Condition.signal ready_for_update;

        Mutex.unlock m;
        Thread.delay !refresh_period
      done
    with Graphics.Graphic_failure _s -> exit 0
  in

  let board_update_loop () =
    while true do
      debug_print "board_update_loop waiting to update";
      Mutex.lock m;
      Condition.wait ready_for_update m;
      if !refresh then (
        Game_of_life.randomize window.game_board; 
        refresh := false;
      );
      debug_print "board_update_loop updating";
      let d = Game_of_life.update_board window.game_board in
      diffs := d;
      Mutex.unlock m;
      Thread.yield ();
      ()
    done
  in

  Arg.parse speclist (fun _ -> ()) usage;

  let drawing_thread = Thread.create drawing_loop () in
  let update_thread = Thread.create board_update_loop () in

  Thread.join drawing_thread;
  Thread.join update_thread
