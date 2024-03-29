let window_width : int = 800
let window_height : int = 600
let player_pos = (1.5, 5.0)
let player_angle = 0.0
let player_speed = 0.004
let player_rot_speed = 0.002

type tmap = { map_width : int; map_height : int; map_list : int list }

module GameMap = struct
  type t = tmap

  let mk () = { map_width = 0; map_height = 0; map_list = [] }

  let set_map map_info lmap =
    map_info :=
      {
        map_width = lmap.map_width;
        map_height = lmap.map_height;
        map_list = lmap.map_list;
      }

  let get_map_info map_info x y =
    let midx = (y * !map_info.map_width) + x in
    midx

  let get_map entities map_info =
    for y = 0 to !map_info.map_height - 1 do
      for x = 0 to !map_info.map_width - 1 do
        let value = List.nth !map_info.map_list (get_map_info map_info x y) in
        if value > 0 then (
          let new_entity = Ecs.Entities.add_entity entities "map" in
          let (pos : Ecs.position_component) =
            { x = float_of_int x *. 50.0; y = float_of_int y *. 50.0 }
          in
          let (shape : Ecs.shape_component) = { width = 50.0; height = 50.0 } in
          Ecs.Entities.update_position entities new_entity.id pos;
          Ecs.Entities.update_shape entities new_entity.id shape;
          ())
        else ()
      done
    done

  let set_player entities =
    let new_entity = Ecs.Entities.add_entity entities "player" in
    let (pos : Ecs.position_component) = { x = 1.5 *. 50.0; y = 2. *. 50. } in
    let (transform : Ecs.transform_component) = { dx = 0.; dy = 0. } in
    let (shape : Ecs.shape_component) = { width = 10.0; height = 10.0 } in
    let (cinput : Ecs.input_component) = { enabled = true } in
    Ecs.Entities.update_position entities new_entity.id pos;
    Ecs.Entities.update_shape entities new_entity.id shape;
    Ecs.Entities.update_cinput entities new_entity.id cinput;
    Ecs.Entities.update_transform entities new_entity.id transform;
    Ecs.Entities.update_animation entities new_entity.id [ "MYCHAR" ] true;
    ()

  let make_panel ?(pos : Ecs.position_component = { x = 0.0; y = 0.0 })
      ?(shape : Ecs.shape_component = { width = 0.0; height = 0.0 })
      ?(panel : Ecs.panel_component = { border = 0.0 })
      ?(text : Ecs.text_component =
        {
          ascii_texture = { texture = None };
          hangul_texture = { texture = None };
          text = "";
          padding = 0.0;
        })
      ?(animation : Ecs.animation_component =
        { frames = []; current_frame = 0; repeatable = false; current_time = 0 })
      entities =
    let new_entity = Ecs.Entities.add_entity entities "panel" in
    match Custom_types.IntMap.find_opt new_entity.id !entities with
    | Some e ->
        let new_entity =
          {
            e with
            pos = Some pos;
            panel = Some panel;
            text = Some text;
            shape = Some shape;
            animation = Some animation;
          }
        in
        entities := Custom_types.IntMap.add new_entity.id new_entity !entities;
        ()
    | None -> ()

  let update entities =
    System2d.update_input entities;
    System2d.update_pos entities;
    System2d.remove_entities entities

  let render entities assets textures renderer =
    System2d.render entities assets textures renderer
end
