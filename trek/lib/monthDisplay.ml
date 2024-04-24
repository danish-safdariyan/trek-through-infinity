(* open Tsdl *)
open Bogue
module W = Widget
module L = Layout
(* module T = Trigger *)

(** Returns layout of task *)
let layout_of_task w task =
  let text = W.text_display task |> L.resident ~x:10 ~y:5 ~w ~h:30 in
  let background = Style.color_bg (Draw.opaque Draw.pale_grey) in
  let line = Style.mk_line ~color:(Draw.opaque Draw.dark_grey) ~width:2 () in
  let border = Style.mk_border ~radius:10 line in
  let box = W.box ~w ~h:30 ~style:(Style.create ~background ~border ()) () in
  L.superpose ~w ~h:30 [ L.resident box; text ]

(** Returns layout of a day with the date and tasks specified. Does not include
    the box. *)
let layout_of_day w date tasks =
  let rec helper = function
    | [] -> []
    | h :: t -> layout_of_task (w - 10) h :: helper t
  in
  let date_marker = W.label date |> L.resident in
  date_marker :: helper tasks |> L.tower ~hmargin:5 ~vmargin:5

let layout_of_month w days =
  let week_layout days =
    let rec helper = function
      | [] -> []
      | h :: t -> layout_of_day w (fst h) (snd h) :: helper t
    in
    let day_infos = helper days in
    let h =
      List.fold_left (fun cur_h day -> max (L.height day) cur_h) 0 day_infos
    in
    let border =
      Style.mk_line ~color:(Draw.opaque Draw.dark_grey) ~width:1 ()
      |> Style.mk_border
    in
    List.map
      (fun day ->
        L.superpose
          [ W.box ~w ~h ~style:(Style.create ~border ()) () |> L.resident; day ])
      day_infos
    |> L.flat ~margins:0
  in
  let rec helper = function
    | [] -> []
    | d1 :: d2 :: d3 :: d4 :: d5 :: d6 :: d7 :: t ->
        week_layout [ d1; d2; d3; d4; d5; d6; d7 ] :: helper t
    | lst -> [ week_layout lst ]
  in
  helper days |> L.tower ~name:"Calendar" ~margins:0 ~scale_content:false
