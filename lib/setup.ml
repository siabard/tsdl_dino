open Tsdl
    
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

  let world_map = Hashtbl.create ((List.length mini_map) * (List.(length (hd mini_map))))

  let get_map () =
    for y = 0 to List.length mini_map - 1 do
      for x = 0 to List.(length (nth mini_map y))- 1 do
        let coord = (x, y) in
        let value = List.(nth (nth mini_map y) x) in
        if value > 0 then 
          Hashtbl.add world_map coord value
        else
          ()
      done
    done

  
  let render_map renderer =
    Hashtbl.iter (fun coord _  ->
        let rect = Some(Sdl.Rect.create ~x:((fst coord)*50) ~y:((snd coord)*50) ~w:50 ~h:50) in
        ignore(Sdl.render_draw_rect renderer rect)
      ) world_map
    
      

end
