open Tsdl
    
type component_class =
  | Position
  | Shape
    

type position_component =
  {
    x: int;
    y: int;
       
  }


type shape_component =
  {
    width: int;
    height: int;
  }

type entity =  {
  id: int;
  tag: string;
  pos: position_component option;
  shape: shape_component option;
}

module IntMap = Map.Make ( Int )

module Entities = struct

  type t = entity list

  let unique_id: int ref = ref 0

  let entities  = ref IntMap.empty 

  let add_entity tag =
    unique_id := !unique_id + 1;
    let new_entity = {
      id = !unique_id;
      tag = tag;
      pos = None;
      shape = None;
    } in
    entities := IntMap.add !unique_id new_entity !entities;
    new_entity


  let update_position (id:int) (new_position: position_component) =
    match IntMap.find_opt id !entities with
    | Some e ->
      let new_entity = {
        id = e.id;
        tag = e.tag;
        pos = Some new_position;
        shape = e.shape;
      } in
      entities := IntMap.add id new_entity !entities;
    | None -> ()

  let update_shape (id: int) (new_shape: shape_component) = 
   match IntMap.find_opt id !entities with
    | Some e ->
      let new_entity = {
        id = e.id;
        tag = e.tag;
        pos = e.pos;
        shape = Some new_shape;
      } in
      entities := IntMap.add id new_entity !entities;
    | None -> ()
    
  let of_tag tag entities = List.filter (fun ele-> ele.tag = tag) entities

  let with_component comp entities =
    match comp with
        | Position -> List.filter (fun ele -> ele.pos <> None) entities
        | Shape -> List.filter (fun ele -> ele.shape <> None) entities
       
  
end


module System2D = struct
  let update_pos (entities: Entities.t): unit =
    List.iter (fun ele ->
        let id = ele.id in 
        let pos = ele.pos in
        match pos with
        | Some pos ->
        let new_pos = { x = pos.x + 1; y = pos.y} in
        Entities.update_position id new_pos
        | None -> ()
      ) entities

  
  let render (renderer: Sdl.renderer ) (entities: Entities.t): unit =
    List.iter (fun ele ->
        match ele.pos with
        | Some {x; y} -> (
          match ele.shape with
          | Some {width; height} ->
            let rect = Sdl.Rect.create ~x:x ~y:y ~w:width ~h: height in
            let _ = Sdl.render_draw_rect renderer (Some rect) in
            ()
          | None -> ()
        )
        | None -> ()
      ) entities
end


