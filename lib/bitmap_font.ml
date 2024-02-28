open Tsdl

let utf8_to_ucs2 (utf8_str : string) : int list =
  let rec utf8_to_chars str idx acc =
    if idx >= String.length str then List.rev acc
    else
      let code_unit = String.get str idx in
      let char_length =
        if code_unit >= '\xC0' && code_unit <= '\xDF' then 2
        else if code_unit >= '\xE0' && code_unit <= '\xEF' then 3
        else if code_unit >= '\xF0' && code_unit <= '\xF7' then 4
        else 1
      in
      let utf8_char = String.sub str idx char_length in
      let ucs2_char =
        match char_length with
        | 1 -> Char.code utf8_char.[0]
        | 2 ->
            ((Char.code utf8_char.[0] land 0x1F) lsl 6)
            lor (Char.code utf8_char.[1] land 0x3F)
        | 3 ->
            ((Char.code utf8_char.[0] land 0x0F) lsl 12)
            lor ((Char.code utf8_char.[1] land 0x3F) lsl 6)
            lor (Char.code utf8_char.[2] land 0x3F)
        | 4 ->
            ((Char.code utf8_char.[0] land 0x07) lsl 18)
            lor ((Char.code utf8_char.[1] land 0x3F) lsl 12)
            lor ((Char.code utf8_char.[2] land 0x3F) lsl 6)
            lor (Char.code utf8_char.[3] land 0x3F)
        | _ -> failwith "Invalid UTF-8 sequence"
      in
      utf8_to_chars str (idx + char_length) (ucs2_char :: acc)
  in
  utf8_to_chars utf8_str 0 []

type han_johab = { cho : int; mid : int; jong : int }

type han_bul = {
  cho_bul : int option;
  mid_bul : int option;
  jong_bul : int option;
}

let charcode_to_han_jaso charcode =
  let num_of_jong = 28 in
  let num_of_mid = 21 in
  let hancode = charcode - 0xac00 in
  let jong = hancode mod num_of_jong in
  let cho = (hancode - jong) / num_of_jong / num_of_mid in
  let mid = (hancode - jong) / num_of_jong mod num_of_mid in
  { cho; mid; jong }

let jaso_to_bul johab =
  match johab with
  | { cho; jong; mid } ->
      if jong = 0 then
        {
          cho_bul =
            (if mid = 20 || (mid >= 0 && mid <= 7) then Some 0
             else if mid = 8 || mid = 12 || mid = 18 then Some 1
             else if mid = 13 || mid = 17 then Some 2
             else if mid = 19 || (mid >= 9 && mid <= 11) then Some 3
             else if mid >= 14 && mid <= 16 then Some 4
             else None);
          mid_bul =
            (if cho = 0 || cho = 1 then Some 0
             else if cho >= 2 && cho <= 18 then Some 1
             else None);
          jong_bul = None;
        }
      else
        {
          cho_bul =
            (if mid = 20 || (mid >= 0 && mid <= 7) then Some 5
             else if mid = 8 || mid = 12 || mid = 13 || mid = 17 || mid = 18
             then Some 6
             else if
               mid = 19 || (mid >= 9 && mid <= 11) || (mid >= 14 && mid <= 16)
             then Some 7
             else None);
          mid_bul =
            (if cho = 0 || cho = 1 then Some 2
             else if cho >= 2 && cho <= 18 then Some 3
             else None);
          jong_bul =
            (if mid = 0 || mid = 2 || mid = 9 then Some 0
             else if
               mid = 4 || mid = 6 || mid = 11 || mid = 14 || mid = 16
               || mid = 19 || mid = 20
             then Some 1
             else if
               mid = 1 || mid = 3 || mid = 5 || mid = 7 || mid = 10 || mid = 15
             then Some 2
             else if mid = 8 || mid = 12 || mid = 13 || mid = 17 || mid = 18
             then Some 3
             else None);
        }

let draw_hangul renderer texture charcode x y =
  let han_width = 16 in
  let han_height = 16 in
  let jaso = charcode_to_han_jaso charcode in
  let bul = jaso_to_bul jaso in
  let cho_rect =
    match bul.cho_bul with
    | Some bul ->
        Sdl.Rect.create ~x:(jaso.cho * han_width) ~y:(bul * han_height)
          ~w:han_width ~h:han_height
    | None -> Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0
  in
  let mid_rect =
    match bul.mid_bul with
    | Some bul ->
        Sdl.Rect.create ~x:(jaso.mid * han_width)
          ~y:((bul + 8) * han_height)
          ~w:han_width ~h:han_height
    | None -> Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0
  in

  let jong_rect =
    match bul.jong_bul with
    | Some bul ->
        Sdl.Rect.create ~x:(jaso.jong * han_width)
          ~y:((bul + 12) * han_height)
          ~w:han_width ~h:han_height
    | None -> Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0
  in
  let dest_rect = Sdl.Rect.create ~x ~y ~w:han_width ~h:han_height in

  ignore
    (Sdl.render_copy_ex ~src:cho_rect ~dst:dest_rect renderer texture 0.0 None
       Sdl.Flip.none);
  ignore
    (Sdl.render_copy_ex ~src:mid_rect ~dst:dest_rect renderer texture 0.0 None
       Sdl.Flip.none);
  ignore
    (Sdl.render_copy_ex ~src:jong_rect ~dst:dest_rect renderer texture 0.0 None
       Sdl.Flip.none);
  ()

let draw_hangul_string renderer textures charcodes x y =
  let han_width = 16 in
  List.iteri
    (fun idx charcode ->
      draw_hangul renderer textures charcode (x + (idx * han_width)) y)
    charcodes
