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

  let (world_map: Ecs.Entity.t list ref) = ref []


  let get_map () =
    for y = 0 to List.length mini_map - 1 do
      for x = 0 to List.(length (nth mini_map y))- 1 do
        let value = List.(nth (nth mini_map y) x) in
        if value > 0 then 
          let entity:Ecs.entity = {
            tag = "map";
            pos = Some { x = x*50; y = y*50};
            shape = Some {width = 50; height = 50}
          } in
          world_map := entity :: (!world_map)
        else
          ()
      done
    done

  
  let render_map renderer =
    let map_entity = Ecs.Entity.of_tag "map" !world_map in
    let position_entity = Ecs.Entity.with_component Position map_entity in
    let shape_entity = Ecs.Entity.with_component Shape position_entity in
    Ecs.System2D.render renderer shape_entity
    
      

end
