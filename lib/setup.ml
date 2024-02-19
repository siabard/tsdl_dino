module Setup = struct
  let window_width :int = 800
    
  let window_height:int = 600

end



module GameMap = struct
  type t = int list list
      
  let mini_map = [[1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1];
                  [1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1];
                  [1; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1];
                  [1; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1];
                  [1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1];
                  [1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1];
                  [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1];
                 ]

  let get_map () =
    for y = 0 to List.length mini_map - 1 do
      for x = 0 to List.(length (nth mini_map y))- 1 do
        let value = List.(nth (nth mini_map y) x) in
        if value > 0 then 
          let new_entity = Ecs.Entities.add_entity "map" in
          let (pos: Ecs.position_component) = { x = x * 50; y = y * 50} in
          let (shape: Ecs.shape_component) = { width = 50; height = 50} in
          Ecs.Entities.update_position new_entity.id pos;
          Ecs.Entities.update_shape new_entity.id shape;
          ()
        else
          ()
      done
    done

  let update_map () =
    let entity_list = Ecs.IntMap.bindings !Ecs.Entities.entities |> List.map snd in
    let position_entity = Ecs.Entities.with_component Position entity_list in
    Ecs.System2D.update_pos position_entity
  
  let render_map renderer =
    let entity_list = Ecs.IntMap.bindings !Ecs.Entities.entities |> List.map snd in
    let map_entity = Ecs.Entities.of_tag "map" entity_list in
    let position_entity = Ecs.Entities.with_component Position map_entity in
    let shape_entity = Ecs.Entities.with_component Shape position_entity in
    Ecs.System2D.render renderer shape_entity
    
      

end
