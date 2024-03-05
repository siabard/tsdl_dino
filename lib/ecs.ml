type component_class = Position | Shape | Input
type transform_component = { dx : float; dy : float }
type position_component = { x : float; y : float }
type shape_component = { width : float; height : float }
type rotation_component = { speed : float; angle : float }
type input_component = { enabled : bool }

type animation_component = {
  frames : string list;
  current_frame : int;
  repeatable : bool;
  current_time : int;
}

type collision_component = {
  center_x : float;
  center_y : float;
  width : float;
  height : float;
}

type entity = {
  id : int;
  is_live : bool;
  tag : string;
  pos : position_component option;
  transform : transform_component option;
  shape : shape_component option;
  rotation : rotation_component option;
  cinput : input_component option;
  collider : collision_component option;
  animation : animation_component option;
}

module Entities = struct
  type t = entity

  let unique_id : int ref = ref 0

  let add_entity entities tag =
    unique_id := !unique_id + 1;
    let new_entity =
      {
        id = !unique_id;
        is_live = true;
        tag;
        pos = None;
        shape = None;
        transform = None;
        rotation = None;
        cinput = None;
        collider = None;
        animation = None;
      }
    in
    entities := Custom_types.IntMap.add !unique_id new_entity !entities;
    new_entity

  let get_list entities = Custom_types.IntMap.bindings !entities |> List.map snd

  let update_cinput entities id new_cinput =
    match Custom_types.IntMap.find_opt id !entities with
    | Some e ->
        let new_entity = { e with cinput = Some new_cinput } in
        entities := Custom_types.IntMap.add id new_entity !entities;
        ()
    | None -> ()

  let update_transform entities id new_transform =
    match Custom_types.IntMap.find_opt id !entities with
    | Some e ->
        let new_entity = { e with transform = Some new_transform } in
        entities := Custom_types.IntMap.add id new_entity !entities;
        ()
    | None -> ()

  let update_position entities id new_position =
    match Custom_types.IntMap.find_opt id !entities with
    | Some e ->
        let new_entity = { e with pos = Some new_position } in
        entities := Custom_types.IntMap.add id new_entity !entities;
        ()
    | None -> ()

  let update_shape entities id new_shape =
    match Custom_types.IntMap.find_opt id !entities with
    | Some e ->
        let new_entity = { e with shape = Some new_shape } in
        entities := Custom_types.IntMap.add id new_entity !entities;
        ()
    | None -> ()

  let update_collider entities id new_collider =
    match Custom_types.IntMap.find_opt id !entities with
    | Some e ->
        let new_entity = { e with collider = Some new_collider } in
        entities := Custom_types.IntMap.add id new_entity !entities;
        ()
    | None -> ()

  let of_tag entities tag =
    let entity_list = get_list entities in
    List.filter (fun ele -> ele.tag = tag) entity_list

  let with_component entities comp =
    let entity_list = get_list !entities in
    match comp with
    | Position -> List.filter (fun ele -> ele.pos <> None) entity_list
    | Shape -> List.filter (fun ele -> ele.shape <> None) entity_list
    | Input -> List.filter (fun ele -> ele.cinput > None) entity_list

  let update_animation entities id frames repeatable =
    match Custom_types.IntMap.find_opt id !entities with
    | Some e ->
        let animation : animation_component =
          { frames; current_frame = 0; repeatable; current_time = 0 }
        in
        let new_entity = { e with animation = Some animation } in
        entities := Custom_types.IntMap.add id new_entity !entities;
        ()
    | None -> ()
end
