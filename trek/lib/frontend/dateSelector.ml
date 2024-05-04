open Bogue
module L = Layout
module W = Widget
module P = Popups

type sel_info = {
  mutable date : Backend.Date.t;
  mutable cur_btn : Button.t;
  mutable on_update : Backend.Date.t -> unit;
}

type t = {
  layout : L.t;
  m_layout : L.t;
  m_label : W.t;
  info : sel_info;
  mutable cur_m : Backend.Date.month;
  mutable cur_y : int;
}

let get_btn info = info.cur_btn
let get_m sel = sel.cur_m
let get_y sel = sel.cur_y

(** Gets a 6-week month. *)
let get_month m y =
  let last = Backend.Date.last_day m y in
  let rec last_week len = function
    | [] -> failwith "Empty list"
    | h :: t ->
        if Backend.Date.day_of_week h = Saturday && len + last.day > 35 then
          h :: t
        else last_week (len + 1) (Backend.Date.next_day h :: h :: t)
  in
  let rec add_month = function
    | [] -> failwith "Empty list"
    | (h : Backend.Date.t) :: t ->
        if h.day = 1 then h :: t
        else add_month (Backend.Date.prev_day h :: h :: t)
  in
  let rec fst_week = function
    | [] -> failwith "Empty list"
    | h :: t ->
        if Backend.Date.day_of_week h = Sunday then h :: t
        else fst_week (Backend.Date.prev_day h :: h :: t)
  in
  last_week 0 [ last ] |> List.rev |> add_month |> fst_week

(** Returns a button layout that represents a day. *)
let layout_of_day (d : Backend.Date.t) m info =
  let s = string_of_int d.day in
  let s = if String.length s < 2 then s ^ " " else s in
  let num = W.label ~align:Draw.Center s in
  let _ =
    if d.month = m then ()
    else Label.set_fg_color (W.get_label num) (Draw.opaque Draw.grey)
  in
  let btn =
    W.button ~kind:Button.Switch ~label:(W.get_label num)
      ~bg_on:(Style.Solid (Draw.opaque Draw.pale_grey))
      ~bg_off:(Style.Solid (Draw.opaque Draw.white))
      ~state:(d = info.date)
      ~border_color:(Draw.opaque Draw.dark_grey)
      "Button"
  in
  let _ =
    if d = info.date then info.cur_btn <- W.get_button btn;
    W.on_button_release
      ~release:(fun w ->
        let btn = W.get_button w in
        if Button.state btn then (
          info.date <- d;
          get_btn info |> Button.reset;
          info.cur_btn <- btn)
        else ();
        info.on_update d)
      btn
  in
  L.resident btn

(** Returns a layout of a month with the specified info. *)
let layout_of_month info m y =
  let days = get_month m y in
  let week_layout days =
    let rec helper info = function
      | [] -> []
      | h :: t -> layout_of_day h m info :: helper info t
    in
    helper info days |> L.flat ~margins:0
  in
  let rec helper = function
    | [] -> []
    | d1 :: d2 :: d3 :: d4 :: d5 :: d6 :: d7 :: t ->
        week_layout [ d1; d2; d3; d4; d5; d6; d7 ] :: helper t
    | lst -> [ week_layout lst ]
  in
  helper days |> L.tower ~margins:0

(** Sets the display of [sel] to the month and year specified. *)
let update_display sel =
  Sync.push (fun () ->
      L.set_rooms sel.m_layout [ layout_of_month sel.info sel.cur_m sel.cur_y ];
      W.set_text sel.m_label
        (Backend.Date.string_of_month sel.cur_m ^ " " ^ string_of_int sel.cur_y))

let make_selector () =
  let info =
    {
      date = Backend.Date.current_date ();
      cur_btn = Button.create Button.Switch "";
      on_update = (fun _ -> ());
    }
  in
  let m_layout = layout_of_month info info.date.month info.date.year in
  let m_label =
    W.label ~align:Draw.Center
      (Backend.Date.string_of_month info.date.month
      ^ " "
      ^ string_of_int info.date.year)
  in
  let prev_btn =
    W.button ~kind:Button.Trigger
      ~label:(Label.create ~align:Draw.Center "<")
      "Prev btn"
  in
  let next_btn =
    W.button ~kind:Button.Trigger
      ~label:(Label.create ~align:Draw.Center ">")
      "Next btn"
  in
  let top = L.flat_of_w [ prev_btn; m_label; next_btn ] in
  let stuff = L.tower [ top; m_layout ] in
  let box =
    W.box ~w:(L.width stuff) ~h:(L.height stuff)
      ~style:
        (Style.create
           ~background:(Style.color_bg (Draw.opaque Draw.white))
           ~border:
             (Style.mk_line ~color:(Draw.opaque Draw.dark_grey) ~width:2 ()
             |> Style.mk_border)
           ())
      ()
    |> L.resident
  in
  let sel =
    {
      layout = L.superpose [ box; stuff ];
      m_layout;
      m_label;
      info;
      cur_m = info.date.month;
      cur_y = info.date.year;
    }
  in
  let _ =
    Space.full_width top;
    W.on_button_release
      ~release:(fun _ ->
        let m, y = Backend.Date.prev_month (get_m sel) (get_y sel) in
        sel.cur_m <- m;
        sel.cur_y <- y;
        update_display sel)
      prev_btn;
    W.on_button_release
      ~release:(fun _ ->
        let m, y = Backend.Date.next_month (get_m sel) (get_y sel) in
        sel.cur_m <- m;
        sel.cur_y <- y;
        update_display sel)
      next_btn
  in
  sel

let get_date sel = sel.info.date
let get_layout sel = sel.layout
let on_update sel f = sel.info.on_update <- f

let set_date sel date =
  sel.info.date <- date;
  sel.cur_m <- date.month;
  sel.cur_y <- date.year;
  update_display sel;
  sel.info.on_update date
