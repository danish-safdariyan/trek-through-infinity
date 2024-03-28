(* open Tsdl *)
open Bogue
open Main
module W = Widget
module L = Layout
(* module T = Trigger *)

let rec draw_horizontal_lines area y w spacing =
  if y >= 0 then
    let _ =
      Sdl_area.draw_line area ~color:(Draw.transp Draw.black) ~thick:3 (0, y)
        (w, y)
    in
    draw_horizontal_lines area (y - spacing) w spacing
  else ()

let rec draw_vertical_lines area x h spacing =
  if x >= 0 then
    let _ =
      Sdl_area.draw_line area ~color:(Draw.transp Draw.black) ~thick:3 (x, 0)
        (x, h)
    in
    draw_vertical_lines area (x - spacing) h spacing
  else ()

let rec to_text_display w h = function
  | [] -> []
  | e :: t -> W.text_display ~w ~h e :: to_text_display w h t

let row_layout b_w b_h days = L.flat_of_w ~sep:0 (to_text_display b_w b_h days)

let rec get_layouts b_w b_h = function
  | [] -> []
  | d1 :: d2 :: d3 :: d4 :: d5 :: d6 :: d7 :: t ->
      row_layout b_w b_h [ d1; d2; d3; d4; d5; d6; d7 ] :: get_layouts b_w b_h t
  | lst -> [ row_layout b_w b_h lst ]

let test lst =
  let w = 150 * 7 in
  let h = 150 * 5 in
  let a = W.sdl_area ~w ~h () in
  let area = W.get_sdl_area a in
  draw_horizontal_lines area h w 150;
  draw_vertical_lines area w h 150;
  let grid = L.resident a in
  let text_layout = L.tower ~sep:0 (get_layouts 150 150 lst) in
  let layout = L.superpose [ grid; text_layout ] in
  run (of_layout layout)
