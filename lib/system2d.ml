open Ecs
open Tsdl
open Textures

let update_input entities =
  let entity_list = Ecs.Entities.get_list entities in
  let new_transform : Ecs.transform_component ref =
    ref { dx = 0.0; dy = 0.0 }
  in
  List.iter
    (fun ele ->
      let id = ele.id in
      let transform = ele.transform in
      let cinput = ele.cinput in
      match (cinput, transform) with
      | Some _, Some _ ->
          (match Hashtbl.find_opt Event.Event.hold_key Event.Event.W with
          | Some 1 ->
              new_transform :=
                { !new_transform with dy = !new_transform.dy -. 1.0 };
              ()
          | _ -> ());
          (match Hashtbl.find_opt Event.Event.hold_key Event.Event.S with
          | Some 1 ->
              new_transform :=
                { !new_transform with dy = !new_transform.dy +. 1.0 };
              ()
          | _ -> ());

          (match Hashtbl.find_opt Event.Event.hold_key Event.Event.A with
          | Some 1 ->
              new_transform :=
                { !new_transform with dx = !new_transform.dx -. 1.0 };
              ()
          | _ -> ());
          (match Hashtbl.find_opt Event.Event.hold_key Event.Event.D with
          | Some 1 ->
              new_transform :=
                { !new_transform with dx = !new_transform.dx +. 1.0 };
              ()
          | _ -> ());

          Ecs.Entities.update_transform entities id !new_transform
      | _, _ -> ())
    entity_list

let update_pos entities =
  let entity_list = Ecs.Entities.get_list entities in
  List.iter
    (fun ele ->
      let id = ele.id in
      let pos = ele.pos in
      let transform = ele.transform in
      match (pos, transform) with
      | Some pos, Some transform ->
          let new_pos =
            { x = pos.x +. transform.dx; y = pos.y +. transform.dy }
          in
          Ecs.Entities.update_position entities id new_pos
      | _, _ -> ())
    entity_list

let remove_entities entities =
  let entity_list = Ecs.Entities.get_list entities in
  List.iter
    (fun ele ->
      if ele.is_live = false then
        entities := Custom_types.IntMap.remove ele.id !entities
      else ())
    entity_list

let render entities assets textures renderer =
  let entity_list = Ecs.Entities.get_list entities in
  List.iter
    (fun ele ->
      match (ele.pos, ele.shape, ele.animation) with
      | Some { x; y }, Some { width; height }, Some { frames; current_frame; _ }
        ->
          let frame = List.nth frames current_frame in
          let rect =
            Sdl.Rect.create ~x:(int_of_float x) ~y:(int_of_float y)
              ~w:(int_of_float width) ~h:(int_of_float height)
          in
          let texture = Custom_types.StringMap.find frame !textures in
          let asset = Custom_types.StringMap.find texture.asset_name !assets in
          let sdl_rect =
            Sdl.Rect.create ~x:(int_of_float texture.x)
              ~y:(int_of_float texture.y) ~w:(int_of_float texture.w)
              ~h:(int_of_float texture.h)
          in
          let _ =
            match asset.texture with
            | Some texture ->
                let _ =
                  Sdl.render_copy_ex ~src:sdl_rect ~dst:rect renderer texture
                    0.0 None Sdl.Flip.none
                in
                ()
            | None -> ()
          in
          ()
      | Some { x; y }, Some { width; height }, None ->
          let rect =
            Sdl.Rect.create ~x:(int_of_float x) ~y:(int_of_float y)
              ~w:(int_of_float width) ~h:(int_of_float height)
          in
          let _ = Sdl.render_draw_rect renderer (Some rect) in
          ()
      | _, _, _ -> ())
    entity_list
