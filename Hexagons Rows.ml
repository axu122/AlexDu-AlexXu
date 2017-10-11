(* file: hexagons.ml
   author: Anran Du

   CSCI 1103 Computer Science I Honors

   A simple program demo-ing Asai's Universe library.
*)
open World
open Image
open Color
open Cs1103

let displayWidth = 800.
let displayHeight = displayWidth
let empty = Image.rectangle displayWidth displayHeight Color.dodgerBlue

type shape = Circle of {radius : float; color : Color.t}
           | Hexagon of {side : float; color : Color.t}

let equilateralHeight side =
  let half = side /. 2.0
  in
  sqrt (side ** 2.0 -. half ** 2.0)

let rec imageOf shape =
  match shape with
  | Circle {radius; color} -> Image.circle radius color

  | Hexagon {side; color} ->
    let height = equilateralHeight side in
    Image.polygon [(side *. 0.5, height *. 1.); (side, 0.); (side *. 0.5, height *. -1.); (side *. -0.5, height *. -1.); (side *. -1., 0.); (side *. -0.5, height)] (Cs1103.randomColor())



(* rowOfHexagons : int -> Image.t
*)
let rowOfHexagons n =
  let side = displayWidth /. (3. *. float n -. 2.) in
  let strip = rectangle displayWidth (side *. sqrt(3.0)) Color.transparent in
  let rec repeat m =
    let hexagon = Hexagon {side = side; color= (Cs1103.randomColor())} in
    let hex = imageOf hexagon in
    match m = 0 with
    |true -> strip
    |false ->
      place_image hex ((3. *. float_of_int m -. 3.) *. side , 0.) (repeat (m - 1))
  in
  repeat n

let draw () =
  let ns = [5] in
  let n = List.nth ns (Random.int (List.length ns))
  in
  rowOfHexagons n

let go () =
  World.big_bang ()
    ~name:"Row of Hexagons"
    ~width:(f2I displayWidth)
    ~height:(f2I displayHeight)
    ~to_draw: draw
    ~rate: 1.0

let s = go ()
