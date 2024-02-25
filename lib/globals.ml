open Setup

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

(** Do Nothing *)
let entities : Ecs.entity Ecs.IntMap.t ref = ref Ecs.IntMap.empty

let map_info : Setup.tmap ref =
  ref { map_width = 0; map_height = 0; map_list = [] }
