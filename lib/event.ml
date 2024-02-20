open Tsdl

module Event = struct
  type key =
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z
    | K1
    | K2
    | K3
    | K4
    | K5
    | K6
    | K7
    | K8
    | K9
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12
    | Enter
    | Space
    | Left
    | Right
    | Up
    | Down
    | Escape
  [@@deriving show, eq]

  type t = Quit | Key of { key : key; down : bool } | NoEvent

  let char_of_key = function
    | A -> 'A'
    | B -> 'B'
    | C -> 'C'
    | D -> 'D'
    | E -> 'E'
    | F -> 'F'
    | G -> 'G'
    | H -> 'H'
    | I -> 'I'
    | J -> 'J'
    | K -> 'K'
    | L -> 'L'
    | M -> 'M'
    | N -> 'N'
    | O -> 'O'
    | P -> 'P'
    | Q -> 'Q'
    | R -> 'R'
    | S -> 'S'
    | T -> 'T'
    | U -> 'U'
    | V -> 'V'
    | W -> 'W'
    | X -> 'X'
    | Y -> 'Y'
    | Z -> 'Z'
    | K1 -> '1'
    | K2 -> '2'
    | K3 -> '3'
    | K4 -> '4'
    | K5 -> '5'
    | K6 -> '6'
    | K7 -> '7'
    | K8 -> '8'
    | K9 -> '9'
    | _ -> failwith "Not a letter"

  let pressed_key : (key, int) Hashtbl.t = Hashtbl.create 54
  let released_key : (key, int) Hashtbl.t = Hashtbl.create 54
  let hold_key : (key, int) Hashtbl.t = Hashtbl.create 54

  let is_letter = function
    | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S
    | T | U | V | W | X | Y | Z ->
        true
    | _ -> false

  let handle_key event event_typ =
    let open Sdl.Event in
    let down =
      match event_typ with
      | `Key_up -> false
      | `Key_down -> true
      | _ -> failwith "Not a key"
    in
    let exception UnhandledKey in
    let key = Sdl.Scancode.enum (get event keyboard_scancode) in
    try
      let key =
        match key with
        | `A -> A
        | `B -> B
        | `C -> C
        | `D -> D
        | `E -> E
        | `F -> F
        | `G -> G
        | `H -> H
        | `I -> I
        | `J -> J
        | `K -> K
        | `L -> L
        | `M -> M
        | `N -> N
        | `O -> O
        | `P -> P
        | `Q -> Q
        | `R -> R
        | `S -> S
        | `T -> T
        | `U -> U
        | `V -> V
        | `W -> W
        | `X -> X
        | `Y -> Y
        | `Z -> Z
        | `K1 -> K1
        | `K2 -> K2
        | `K3 -> K3
        | `K4 -> K4
        | `K5 -> K5
        | `K6 -> K6
        | `K7 -> K7
        | `K8 -> K8
        | `K9 -> K9
        | `F1 -> F1
        | `F2 -> F2
        | `F3 -> F3
        | `F4 -> F4
        | `F5 -> F5
        | `F6 -> F6
        | `F7 -> F7
        | `F8 -> F8
        | `F9 -> F9
        | `F10 -> F10
        | `F11 -> F11
        | `F12 -> F12
        | `Return -> Enter
        | `Space -> Space
        | `Left -> Left
        | `Right -> Right
        | `Up -> Up
        | `Down -> Down
        | `Escape -> Escape
        | _ -> raise_notrace UnhandledKey
      in
      Key { down; key }
    with UnhandledKey -> NoEvent

  let clear_key () =
    Hashtbl.clear pressed_key;
    Hashtbl.clear released_key;
    ()

  let of_sdl event =
    let t = Sdl.Event.(enum (get event typ)) in
    match t with
    | `Quit -> Quit
    | `Key_down | `Key_up -> handle_key event t
    | _ -> NoEvent
end
