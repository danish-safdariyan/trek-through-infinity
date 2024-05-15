open Backend
open Bogue
module L = Layout
module W = Widget

let layout_of_task task =
  let label = L.resident ~x:5 ~y:5 task in
  let background = Style.color_bg (Draw.opaque (Draw.find_color "#99e3e4")) in
  let line =
    Style.mk_line ~color:(Draw.opaque (Draw.find_color "#008284")) ~width:2 ()
  in
  let border = Style.mk_border ~radius:10 line in
  let box = W.box ~w:80 ~h:30 ~style:(Style.create ~background ~border ()) () in
  let out = L.superpose ~w:80 ~h:30 [ L.resident box; label ] in
  out

let layout_of_taskList = ()

let taskListLayout tList =
  let tList = [ "Test"; "Test"; "Test"; "Test" ] in
  let label = W.label ~size:18 "Tasks" |> L.resident in
  let task_list = List.map (fun task -> layout_of_task (W.label task)) tList in
  let header =
    L.flat
      [ label; W.button "Add" |> L.resident; W.button "Clear" |> L.resident ]
  in
  let room_list = header :: task_list in
  let taskLayout =
    L.superpose
      [
        GenDisplay.surrounding_box ~width:3 (L.tower room_list);
        L.tower room_list;
      ]
  in
  let () =
    List.iter
      (fun ch -> Space.full_width ~right_margin:10 ~left_margin:10 ch)
      room_list
  in
  let () = Space.full_width taskLayout in
  taskLayout
