open Tsdl
open Tsdl_image
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
  let map_height = List.length mini_map in
  let map_width = List.(length (hd mini_map)) in
  let map_list = List.flatten mini_map in
  let map_info = Tsdl_dino.Game_map.map_info in
  let entities = ref Tsdl_dino.Custom_types.IntMap.empty in
  let assets = ref Tsdl_dino.Custom_types.StringMap.empty in
  let textures = ref Tsdl_dino.Custom_types.StringMap.empty in
  match Sdl.init Sdl.Init.(video + events) with
  | Error (`Msg e) ->
      Sdl.log "Init error: %s" e;
      exit 1
  | Ok () -> (
      ignore Image.Init.(jpg + png);
      let current_tick = ref (Sdl.get_ticks ()) in
      let last_tick = ref !current_tick in
      let event = Sdl.Event.create () in
      let some_event = Some event in
      match
        Sdl.create_window ~w:Tsdl_dino.Game_map.window_width
          ~h:Tsdl_dino.Game_map.window_height "SDL OpenGL"
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
              ignore (Sdl.set_hint Sdl.Hint.render_scale_quality "linear");
              Tsdl_dino.Game_map.GameMap.set_map map_info
                { map_width; map_height; map_list };
              Tsdl_dino.Textures.load_asset r assets "assets/hangul.png"
                "hangul";
              Tsdl_dino.Textures.load_asset r assets "assets/ascii.png" "ascii";
              Tsdl_dino.Textures.load_asset r assets "assets/mychar.png" "char";
              Tsdl_dino.Textures.add_texture textures "char" "MYCHAR" 0.0 0.0
                16.0 16.0;
              Tsdl_dino.Game_map.GameMap.get_map entities map_info;
              Tsdl_dino.Game_map.GameMap.set_player entities;
              let rec game_loop () =
                ignore (Tsdl_dino.Event.Event.clear_key ());
                last_tick := !current_tick;
                let event =
                  if Sdl.poll_event some_event then Event.of_sdl event
                  else Event.NoEvent
                in
                (match event with
                | Event.Quit ->
                    Tsdl_dino.Textures.clear_assets !assets;
                    Sdl.destroy_renderer r;
                    Sdl.destroy_window w;
                    Image.quit ();
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

                Tsdl_dino.Game_map.GameMap.update entities;
                ignore (Sdl.set_render_draw_color r 0x10 0x10 0x10 0xff);
                ignore (Sdl.render_clear r);
                ignore (Sdl.set_render_draw_color r 0xff 0xff 0xff 0xff);
                let ascii_asset =
                  Tsdl_dino.Custom_types.StringMap.find "ascii" !assets
                in
                let hangul_asset =
                  Tsdl_dino.Custom_types.StringMap.find "hangul" !assets
                in
                (match (ascii_asset, hangul_asset) with
                | ( { texture = Some ascii_texture },
                    { texture = Some hangul_texture } ) ->
                    Tsdl_dino.Bitmap_font.draw_string r ascii_texture
                      hangul_texture
                      (Tsdl_dino.Bitmap_font.utf8_to_ucs2
                         "안녕하세요? 한국... Hello World")
                      20 140
                | _ -> ());

                Tsdl_dino.Game_map.GameMap.render entities assets textures r;
                Sdl.render_present r;
                current_tick := Sdl.get_ticks ();
                let dt = Int32.sub !current_tick !last_tick in
                if Int32.(compare dt 33l) < 0 then Sdl.delay Int32.(sub 33l dt)
                else ();

                game_loop ()
              in
              game_loop ()))

let () = main ()
