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

let border_color = Draw.opaque (Draw.find_color "#00B9BC")
let min_h = 100

(** Returns layout of a day with the date and tasks specified. Does not include
    the box. *)
let layout_of_day w m ((date : Date.t), tasks) layout update_calendar =
  let rec helper = function
    | [] -> []
    | h :: t ->
        EventDisplay.layout_of_event (w - 10) date h layout update_calendar
        :: helper t
  in
  let date_marker =
    W.label
      ~fg:(Draw.opaque (if date.month = m then Draw.black else Draw.pale_grey))
      (string_of_int date.day)
  in
  L.resident date_marker :: helper tasks
  |> L.tower ~name:(Date.format_date date) ~hmargin:5 ~vmargin:5

let header w =
  let header_lst = [ "SUN"; "MON"; "TUE"; "WED"; "THU"; "FRI"; "SAT" ] in
  let headers =
    List.map
      (fun s ->
        let label = W.label ~size:20 ~align:Draw.Center s |> L.resident in
        let _ = Space.full_width label in
        L.superpose
          [
            W.box ~w ~h:30
              ~style:
                (Style.create
                   ~border:
                     (Style.mk_line ~color:border_color ~width:2 ()
                     |> Style.mk_border)
                   ())
              ()
            |> L.resident;
            label;
          ])
      header_lst
  in
  L.flat ~margins:0 headers

let layout_of_month w cal month update_calendar =
  let background_layout = L.empty ~w:(w * 7) ~h:(min_h * 5) () in
  let days = get_month_tasks cal month.days in
  let week_layout days =
    let rec helper = function
      | [] -> []
      | h :: t ->
          layout_of_day w month.m h background_layout update_calendar
          :: helper t
    in
    let day_infos = helper days in
    let h =
      List.fold_left (fun cur_h day -> max (L.height day) cur_h) min_h day_infos
    in
    let border =
      Style.mk_line ~color:border_color ~width:2 () |> Style.mk_border
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
  let month_layout =
    L.superpose [ background_layout; helper days |> L.tower ~margins:0 ]
  in
  let _ =
    Space.full_width label;
    L.set_height background_layout (L.height month_layout)
  in
  L.tower ~name:"Calendar" [ label; header w; month_layout ]
