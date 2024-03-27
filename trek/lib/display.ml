(* open Tsdl *)
open Bogue
open Main
module W = Widget
module L = Layout
(* module T = Trigger *)

let rec draw_horizontal_lines area y w spacing = 
  if y >= 0 then 
    let _ = Sdl_area.draw_line area ~color:(Draw.transp Draw.black) ~thick:3 (0, y) (w, y) in
    draw_horizontal_lines area (y-spacing) w spacing
  else ()
let rec draw_vertical_lines area x h spacing = 
  if x >= 0 then 
    let _ = Sdl_area.draw_line area ~color:(Draw.transp Draw.black) ~thick:3 (x, 0) (x, h) in
    draw_vertical_lines area (x-spacing) h spacing
  else ()

let test () =
  let w = 1400 in
  let h = 1000 in
  let a = W.sdl_area ~w ~h () in
  let area = W.get_sdl_area a in
  draw_horizontal_lines area h w 200;
  draw_vertical_lines area w h 200;
  let layout = L.resident a in
  run (of_layout layout)
