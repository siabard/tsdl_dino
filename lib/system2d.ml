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
      match (ele.pos, ele.shape, ele.animation, ele.panel, ele.text) with
      | ( Some { x; y },
          Some { width; height },
          Some { frames; current_frame; _ },
          Some { border },
          Some { text; padding; _ } ) ->
          let rect_tl =
            Sdl.Rect.create ~x:(int_of_float x) ~y:(int_of_float y)
              ~w:(int_of_float border) ~h:(int_of_float border)
          in
          let rect_tm =
            Sdl.Rect.create
              ~x:(int_of_float (x +. border))
              ~y:(int_of_float y)
              ~w:(int_of_float (width -. (border *. 2.0)))
              ~h:(int_of_float border)
          in
          let rect_tr =
            Sdl.Rect.create
              ~x:(int_of_float (x +. width -. (border *. 2.0)))
              ~y:(int_of_float y) ~w:(int_of_float border)
              ~h:(int_of_float border)
          in
          let rect_ml =
            Sdl.Rect.create ~x:(int_of_float x)
              ~y:(int_of_float (y +. border))
              ~w:(int_of_float border)
              ~h:(int_of_float (height -. (border *. 2.0)))
          in
          let rect_mm =
            Sdl.Rect.create
              ~x:(int_of_float (x +. border))
              ~y:(int_of_float (y +. border))
              ~w:(int_of_float (width -. (border *. 2.0)))
              ~h:(int_of_float (height -. (border *. 2.0)))
          in
          let rect_mr =
            Sdl.Rect.create
              ~x:(int_of_float (x +. width -. (border *. 2.0)))
              ~y:(int_of_float (y +. border))
              ~w:(int_of_float border)
              ~h:(int_of_float (height -. (border *. 2.0)))
          in
          let rect_bl =
            Sdl.Rect.create ~x:(int_of_float x)
              ~y:(int_of_float (y +. height -. (border *. 2.0)))
              ~w:(int_of_float border) ~h:(int_of_float border)
          in
          let rect_bm =
            Sdl.Rect.create
              ~x:(int_of_float (x +. border))
              ~y:(int_of_float (y +. height -. (border *. 2.0)))
              ~w:(int_of_float (width -. (border *. 2.0)))
              ~h:(int_of_float border)
          in
          let rect_br =
            Sdl.Rect.create
              ~x:(int_of_float (x +. width -. (border *. 2.0)))
              ~y:(int_of_float (y +. height -. (border *. 2.0)))
              ~w:(int_of_float border) ~h:(int_of_float border)
          in
          let ascii_asset = Custom_types.StringMap.find "ascii" !assets in
          let hangul_asset = Custom_types.StringMap.find "hangul" !assets in
          let frame = List.nth frames current_frame in
          let texture = Custom_types.StringMap.find frame !textures in
          let asset = Custom_types.StringMap.find texture.asset_name !assets in
          (match (ascii_asset.texture, hangul_asset.texture, asset.texture) with
          | Some ascii_texture, Some hangul_texture, Some asset_texure ->
              ignore
                (Sdl.render_copy_ex
                   ~src:
                     (Sdl.Rect.create ~x:0 ~y:0 ~w:(int_of_float border)
                        ~h:(int_of_float border))
                   ~dst:rect_tl renderer asset_texure 0.0 None Sdl.Flip.none);
              ignore
                (Sdl.render_copy_ex
                   ~src:
                     (Sdl.Rect.create ~x:(int_of_float border) ~y:0
                        ~w:(int_of_float border) ~h:(int_of_float border))
                   ~dst:rect_tm renderer asset_texure 0.0 None Sdl.Flip.none);
              ignore
                (Sdl.render_copy_ex
                   ~src:
                     (Sdl.Rect.create
                        ~x:(int_of_float border * 2)
                        ~y:0 ~w:(int_of_float border) ~h:(int_of_float border))
                   ~dst:rect_tr renderer asset_texure 0.0 None Sdl.Flip.none);
              ignore
                (Sdl.render_copy_ex
                   ~src:
                     (Sdl.Rect.create ~x:0 ~y:(int_of_float border)
                        ~w:(int_of_float border) ~h:(int_of_float border))
                   ~dst:rect_ml renderer asset_texure 0.0 None Sdl.Flip.none);
              ignore
                (Sdl.render_copy_ex
                   ~src:
                     (Sdl.Rect.create ~x:(int_of_float border)
                        ~y:(int_of_float border) ~w:(int_of_float border)
                        ~h:(int_of_float border))
                   ~dst:rect_mm renderer asset_texure 0.0 None Sdl.Flip.none);
              ignore
                (Sdl.render_copy_ex
                   ~src:
                     (Sdl.Rect.create
                        ~x:(int_of_float border * 2)
                        ~y:(int_of_float border) ~w:(int_of_float border)
                        ~h:(int_of_float border))
                   ~dst:rect_mr renderer asset_texure 0.0 None Sdl.Flip.none);
              ignore
                (Sdl.render_copy_ex
                   ~src:
                     (Sdl.Rect.create ~x:0
                        ~y:(int_of_float border * 2)
                        ~w:(int_of_float border) ~h:(int_of_float border))
                   ~dst:rect_bl renderer asset_texure 0.0 None Sdl.Flip.none);
              ignore
                (Sdl.render_copy_ex
                   ~src:
                     (Sdl.Rect.create ~x:(int_of_float border)
                        ~y:(int_of_float border * 2)
                        ~w:(int_of_float border) ~h:(int_of_float border))
                   ~dst:rect_bm renderer asset_texure 0.0 None Sdl.Flip.none);
              ignore
                (Sdl.render_copy_ex
                   ~src:
                     (Sdl.Rect.create
                        ~x:(int_of_float border * 2)
                        ~y:(int_of_float border * 2)
                        ~w:(int_of_float border) ~h:(int_of_float border))
                   ~dst:rect_br renderer asset_texure 0.0 None Sdl.Flip.none);
              ignore
                (Bitmap_font.draw_string renderer ascii_texture hangul_texture
                   (Bitmap_font.utf8_to_ucs2 text)
                   (int_of_float (x +. padding))
                   (int_of_float (y +. padding)))
          | _ -> ());
          ()
      | ( Some { x; y },
          Some { width; height },
          Some { frames; current_frame; _ },
          _,
          _ ) ->
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
      | Some { x; y }, Some { width; height }, _, _, _ ->
          let rect =
            Sdl.Rect.create ~x:(int_of_float x) ~y:(int_of_float y)
              ~w:(int_of_float width) ~h:(int_of_float height)
          in
          let _ = Sdl.render_draw_rect renderer (Some rect) in
          ()
      | _, _, _, _, _ -> ())
    entity_list
