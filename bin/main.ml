open Tsdl
module Event = Tsdl_dino.Event.Event

let main () =
  let mini_map =
    [
      [ 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 ];
      [ 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1 ];
      [ 1; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1 ];
      [ 1; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1 ];
      [ 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1 ];
      [ 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1 ];
      [ 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 ];
    ]
  in
  let entities = ref Tsdl_dino.Ecs.IntMap.empty in
  match Sdl.init Sdl.Init.(video + events) with
  | Error (`Msg e) ->
      Sdl.log "Init error: %s" e;
      exit 1
  | Ok () -> (
      let current_tick = ref (Sdl.get_ticks ()) in
      let last_tick = ref !current_tick in
      let event = Sdl.Event.create () in
      let some_event = Some event in
      match
        Sdl.create_window ~w:Tsdl_dino.Setup.window_width
          ~h:Tsdl_dino.Setup.window_height "SDL OpenGL"
          Sdl.Window.(shown + opengl)
      with
      | Error (`Msg e) ->
          Sdl.log "Create window error: %s" e;
          exit 1
      | Ok w -> (
          match
            Sdl.create_renderer
              ~flags:Sdl.Renderer.(accelerated + targettexture)
              w
          with
          | Error (`Msg e) ->
              Sdl.log "Create renderer error: %s" e;
              exit 1
          | Ok r ->
              Tsdl_dino.Setup.GameMap.get_map entities mini_map;
              Tsdl_dino.Setup.GameMap.set_player entities;
              let rec game_loop () =
                ignore (Tsdl_dino.Event.Event.clear_key ());
                last_tick := !current_tick;
                let event =
                  if Sdl.poll_event some_event then Event.of_sdl event
                  else Event.NoEvent
                in
                (match event with
                | Event.Quit ->
                    Sdl.destroy_renderer r;
                    Sdl.destroy_window w;
                    Sdl.quit ();
                    exit 0
                | Event.Key { key; down } ->
                    if down then (
                      Hashtbl.replace Event.pressed_key key 1;
                      Hashtbl.replace Event.hold_key key 1)
                    else (
                      Hashtbl.replace Event.released_key key 1;
                      Hashtbl.remove Event.hold_key key)
                | _ -> ());


                Tsdl_dino.Setup.GameMap.update_map entities;
                ignore (Sdl.set_render_draw_color r 0x10 0x10 0x10 0xff);
                ignore (Sdl.render_clear r);
                ignore (Sdl.set_render_draw_color r 0xff 0xff 0xff 0xff);
                Tsdl_dino.Setup.GameMap.render_map entities r;
                Sdl.render_present r;
                current_tick := Sdl.get_ticks ();
                let dt = Int32.sub !current_tick !last_tick in
                if Int32.(compare dt 33l) < 0 then Sdl.delay Int32.(sub 33l dt)
                else ();

                game_loop ()
              in
              game_loop ()))

let () = main ()
