let window_width : int = 800
let window_height : int = 600
let player_pos = (1.5, 5.0)
let player_angle = 0.0
let player_speed = 0.004
let player_rot_speed = 0.002

module GameMap = struct
  type t = int list list

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

  let get_map () =
    for y = 0 to List.length mini_map - 1 do
      for x = 0 to List.(length (nth mini_map y)) - 1 do
        let value = List.(nth (nth mini_map y) x) in
        if value > 0 then (
          let new_entity = Ecs.Entities.add_entity "map" in
          let (pos : Ecs.position_component) =
            { x = float_of_int x *. 50.0; y = float_of_int y *. 50.0 }
          in
          let (shape : Ecs.shape_component) = { width = 50.0; height = 50.0 } in
          Ecs.Entities.update_position new_entity.id pos;
          Ecs.Entities.update_shape new_entity.id shape;
          ())
        else ()
      done
    done

  let set_player () =
    let new_entity = Ecs.Entities.add_entity "player" in
    let (pos : Ecs.position_component) = { x = 1.5 *. 50.0; y = 2. *. 50. } in
    let (transform: Ecs.transform_component) = {dx = 0.; dy = 0.} in 
    let (shape : Ecs.shape_component) = { width = 10.0; height = 10.0 } in
    let (cinput : Ecs.input_component) = { enabled = true } in
    Ecs.Entities.update_position new_entity.id pos;
    Ecs.Entities.update_shape new_entity.id shape;
    Ecs.Entities.update_cinput new_entity.id cinput;
    Ecs.Entities.update_transform new_entity.id transform;
    ()

  let update_map () =
    let entity_list =
      Ecs.IntMap.bindings !Ecs.Entities.entities |> List.map snd
    in
    let cinput_entity = Ecs.Entities.with_component Input entity_list in
    let position_entity = Ecs.Entities.with_component Position entity_list in
    Ecs.System2D.update_input cinput_entity;
    Ecs.System2D.update_pos position_entity

  let render_map renderer =
    let entity_list =
      Ecs.IntMap.bindings !Ecs.Entities.entities |> List.map snd
    in
    let position_entity = Ecs.Entities.with_component Position entity_list in
    let shape_entity = Ecs.Entities.with_component Shape position_entity in
    Ecs.System2D.render renderer shape_entity
end
