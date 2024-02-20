open Tsdl

type component_class = Position | Shape | Input
type transform_component = { dx : float; dy : float }
type position_component = { x : float; y : float }
type shape_component = { width : float; height : float }
type rotation_component = { speed : float; angle : float }
type input_component = { enabled : bool }

type entity = {
  id : int;
  tag : string;
  pos : position_component option;
  transform : transform_component option;
  shape : shape_component option;
  rotation : rotation_component option;
  cinput : input_component option;
}

module IntMap = Map.Make (Int)

module Entities = struct
  type t = entity list

  let unique_id : int ref = ref 0
  let entities = ref IntMap.empty

  let add_entity tag =
    unique_id := !unique_id + 1;
    let new_entity =
      {
        id = !unique_id;
        tag;
        pos = None;
        shape = None;
        transform = None;
        rotation = None;
        cinput = None;
      }
    in
    entities := IntMap.add !unique_id new_entity !entities;
    new_entity

  let update_cinput id new_cinput =
    match IntMap.find_opt id !entities with
    | Some e ->
        let new_entity =
          {
            id = e.id;
            tag = e.tag;
            pos = e.pos;
            shape = e.shape;
            transform = e.transform;
            rotation = e.rotation;
            cinput = Some new_cinput;
          }
        in
        entities := IntMap.add id new_entity !entities;
        ()
    | None -> ()

  let update_transform id new_transform =
    match IntMap.find_opt id !entities with
    | Some e ->
        let new_entity =
          {
            id = e.id;
            tag = e.tag;
            pos = e.pos;
            shape = e.shape;
            transform = Some new_transform;
            rotation = e.rotation;
            cinput = e.cinput;
          }
        in
        entities := IntMap.add id new_entity !entities;
        ()
    | None -> ()

  let update_position (id : int) (new_position : position_component) =
    match IntMap.find_opt id !entities with
    | Some e ->
        let new_entity =
          {
            id = e.id;
            tag = e.tag;
            pos = Some new_position;
            shape = e.shape;
            transform = e.transform;
            rotation = e.rotation;
            cinput = e.cinput;
          }
        in
        entities := IntMap.add id new_entity !entities;
        ()
    | None -> ()

  let update_shape (id : int) (new_shape : shape_component) =
    match IntMap.find_opt id !entities with
    | Some e ->
        let new_entity =
          {
            id = e.id;
            tag = e.tag;
            pos = e.pos;
            shape = Some new_shape;
            transform = e.transform;
            rotation = e.rotation;
            cinput = e.cinput;
          }
        in
        entities := IntMap.add id new_entity !entities
    | None -> ()

  let of_tag tag entities = List.filter (fun ele -> ele.tag = tag) entities

  let with_component comp entities =
    match comp with
    | Position -> List.filter (fun ele -> ele.pos <> None) entities
    | Shape -> List.filter (fun ele -> ele.shape <> None) entities
    | Input -> List.filter (fun ele -> ele.cinput > None) entities
end

module System2D = struct
  let update_input entities =
    List.iter
      (fun ele ->
        let id = ele.id in
        let transform = ele.transform in
        let cinput = ele.cinput in
        match cinput with
        | Some _ -> (
            match Hashtbl.find_opt Event.Event.hold_key Event.Event.W with
            | Some _ -> (
                match transform with
                | Some _ ->
                    let new_transform = { dx = 1.0; dy = 1.0 } in
                    Entities.update_transform id new_transform
                | _ -> ())
            | _ -> ();                   
          )
        | None -> ())
      entities

  let update_pos (entities : Entities.t) : unit =
    List.iter
      (fun ele ->
        let id = ele.id in
        let pos = ele.pos in
        let transform = ele.transform in
        match pos with
        | Some pos -> (
            match transform with
            | Some transform ->
                let new_pos =
                  { x = pos.x +. transform.dx; y = pos.y +. transform.dy }
                in
                Entities.update_position id new_pos
            | None -> ())
        | None -> ())
      entities

  let render (renderer : Sdl.renderer) (entities : Entities.t) : unit =
    List.iter
      (fun ele ->
        match ele.pos with
        | Some { x; y } -> (
            match ele.shape with
            | Some { width; height } ->
                let rect =
                  Sdl.Rect.create ~x:(int_of_float x) ~y:(int_of_float y)
                    ~w:(int_of_float width) ~h:(int_of_float height)
                in
                let _ = Sdl.render_draw_rect renderer (Some rect) in
                ()
            | None -> ())
        | None -> ())
      entities
end
