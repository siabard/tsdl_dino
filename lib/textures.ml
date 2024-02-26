(**
   텍스쳐를 구성하는 모듈
*)

open Tsdl
open Result
module StringMap = Map.Make (String)

type tassets = { texture : Sdl.texture option }

let add_textures renderer assets pathname name =
  match Tsdl_image.Image.load_texture renderer pathname with
  | Ok texture ->
      let asset = { texture = Some texture } in
      assets := StringMap.add name asset !assets;
      ()
  | Error _ ->
      Printf.printf "FAIL TO OPEN %s" name;
      ()

let clear_assets assets =
  let asset_list = StringMap.bindings assets |> List.map snd in
  List.iter
    (fun asset ->
      match asset with
      | { texture = Some texture } ->
          Sdl.destroy_texture texture;
          ()
      | _ -> ())
    asset_list
