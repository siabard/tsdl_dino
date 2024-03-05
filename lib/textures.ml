(**
   텍스쳐를 구성하는 모듈
*)

open Tsdl
open Result

type tassets = { texture : Sdl.texture option }

type ttexture = {
  asset_name : string;
  texture_name : string;
  x : float;
  y : float;
  w : float;
  h : float;
}

let load_asset renderer assets pathname name =
  match Tsdl_image.Image.load_texture renderer pathname with
  | Ok texture ->
      let asset = { texture = Some texture } in
      assets := Custom_types.StringMap.add name asset !assets;
      ()
  | Error _ ->
      Printf.printf "FAIL TO OPEN %s" name;
      ()

let clear_assets assets =
  let asset_list = Custom_types.StringMap.bindings assets |> List.map snd in
  List.iter
    (fun asset ->
      match asset with
      | { texture = Some texture } ->
          Sdl.destroy_texture texture;
          ()
      | _ -> ())
    asset_list

let add_texture textures asset_name texture_name x y w h =
  textures :=
    Custom_types.StringMap.add texture_name
      { asset_name; texture_name; x; y; w; h }
      !textures
