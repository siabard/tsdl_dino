open Tsdl


module Event = struct
  type t =
    | Quit
    | NoEvent

  let of_sdl event =
    let t = Sdl.Event.(enum (get event typ)) in
    match t with
    | `Quit -> Quit
    | _ -> NoEvent
end 

let main () = match Sdl.init Sdl.Init.(video + events) with
| Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
| Ok () ->
  let event = Sdl.Event.create () in
  let some_event = Some event in
    match Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
      match Sdl.create_renderer ~flags:Sdl.Renderer.(accelerated + presentvsync + targettexture) w with
      | Error (`Msg e) -> Sdl.log "Create renderer error: %s" e; exit 1
      | Ok r -> 
        let rec event_loop () =
          let event =
            if Sdl.poll_event some_event then Event.of_sdl event else Event.NoEvent
          in 
          match event with
          | Event.Quit ->
            Sdl.destroy_renderer r;
            Sdl.destroy_window w;
            Sdl.quit ();
            exit 0
          | _ ->
            Sdl.delay 300l;
            ignore(Sdl.set_render_draw_color r 0x50 0xc8 0x78 0xff);
            ignore(Sdl.render_clear r);
            Sdl.render_present r;
            event_loop ()
        in
        event_loop ()

let () = main ()
