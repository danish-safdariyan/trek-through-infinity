open Backend

type t = {
  days : Date.t list;
  m : Date.month;
  y : int;
}

let get_month m y =
  let lst = [ Date.last_day m y ] in
  let rec last_week = function
    | [] -> failwith "Empty list"
    | h :: t ->
        if Date.day_of_week h = Saturday then h :: t
        else last_week (Date.next_day h :: h :: t)
  in
  let rec add_month = function
    | [] -> failwith "Empty list"
    | (h : Date.t) :: t ->
        if h.day = 1 then h :: t else add_month (Date.prev_day h :: h :: t)
  in
  let rec fst_week = function
    | [] -> failwith "Empty list"
    | h :: t ->
        if Date.day_of_week h = Sunday then h :: t
        else fst_week (Date.prev_day h :: h :: t)
  in
  { days = last_week lst |> List.rev |> add_month |> fst_week; m; y }

let get_month_tasks cal =
  List.map (fun (d : Date.t) -> (d, Calendar.find_events cal d))

let get_month_info month = (month.m, month.y)
let string_of_month m = Date.string_of_month m.m ^ string_of_int m.y

open Bogue
module W = Widget
module L = Layout

(**[layout_of_task w task] returns a layout of a [task] which is a [string].
   [task] is supposed to be inputted by the user and [w] is supposed to be
   passed down and is a ratio of the current window size. Ultimately returns
   layout of task. *)
let layout_of_task w task =
  let text =
    Event.get_title task |> W.text_display |> L.resident ~x:10 ~y:5 ~w ~h:30
  in
  let background = Style.color_bg (Draw.opaque Draw.pale_grey) in
  let line = Style.mk_line ~color:(Draw.opaque Draw.dark_grey) ~width:2 () in
  let border = Style.mk_border ~radius:10 line in
  let box = W.box ~w ~h:30 ~style:(Style.create ~background ~border ()) () in
  L.superpose ~name:"Task" ~w ~h:30 [ L.resident box; text ]

(** Returns layout of a day with the date and tasks specified. Does not include
    the box. *)
let layout_of_day w m (date : Date.t) tasks =
  let rec helper = function
    | [] -> []
    | h :: t -> layout_of_task (w - 10) h :: helper t
  in
  let date_marker =
    W.label
      ~fg:(Draw.opaque (if date.month = m then Draw.black else Draw.pale_grey))
      (string_of_int date.day)
  in
  L.resident date_marker :: helper tasks
  |> L.tower ~name:(Date.format_date date) ~hmargin:5 ~vmargin:5

let min_h = 100

let layout_of_month w cal month =
  let days = get_month_tasks cal month.days in
  let week_layout days =
    let rec helper = function
      | [] -> []
      | h :: t -> layout_of_day w month.m (fst h) (snd h) :: helper t
    in
    let day_infos = helper days in
    let h =
      List.fold_left (fun cur_h day -> max (L.height day) cur_h) min_h day_infos
    in
    let border =
      Style.mk_line ~color:(Draw.opaque (Draw.find_color "#00B9BC")) ~width:1 ()
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
  let label =
    W.label ~size:20 ~align:Draw.Center (string_of_month month)
    |> L.resident ~name:"Month Label"
  in
  let _ = Space.full_width label in
  label :: helper days |> L.tower ~name:"Calendar" ~margins:0
