open Tsdl

type component_class =
  | Position
  | Shape
    

type position_component =
  {    x: int;  y: int;
       
  }


type shape_component = {
  width: int;
  height: int;
}

type entity =  {
    tag: string;
    pos: position_component option;
    shape: shape_component option;
  }
  
module Entity = struct

  type t = entity


  let of_tag tag entities = List.filter (fun ele-> ele.tag = tag) entities

  let with_component comp entities =
    match comp with
        | Position -> List.filter (fun ele -> ele.pos <> None) entities
        | Shape -> List.filter (fun ele -> ele.shape <> None) entities
       
  
end


module System2D = struct
  let render (renderer: Sdl.renderer ) (entities: entity list): unit =
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


