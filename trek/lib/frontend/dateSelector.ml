open Bogue
open Backend
open GenDisplay
module L = Layout
module W = Widget
module P = Popups

type sel_info = {
  mutable date : Date.t;
  mutable cur_btn : Box.t;
  mutable on_update : Date.t -> unit;
}

module type BtnSig = sig
  type t

  val make_btn : Date.t -> Date.month -> sel_info -> t
  (** Makes a button for the date *)

  val get_layout : t -> L.t
  (** Returns the layout associated with the button *)

  val change_btn : t -> Date.t -> Date.month -> sel_info -> unit
  (** Changes the button into the button with the provided info; layout will
      update. *)

  val empty_box : Box.t
  (** An empty box. *)
end

module Btn : BtnSig = struct
  type t = {
    layout : L.t;
    label : W.t;
    box : Box.t;
    mutable d : Date.t;
  }

  let w, h, size = (20, 20, 15)

  (** Style of box when this is not the selected button. *)
  let off_style =
    Style.create
      ~background:(Draw.opaque Draw.white |> Style.color_bg)
      ~border:
        (Style.mk_line ~color:(Draw.opaque Draw.dark_grey) ~width:2 ()
        |> Style.mk_border)
      ()

  (** Style of box when this is the selected button. *)
  let on_style =
    Style.create
      ~background:(Draw.opaque Draw.pale_grey |> Style.color_bg)
      ~border:
        (Style.mk_line ~color:(Draw.opaque Draw.dark_grey) ~width:2 ()
        |> Style.mk_border)
      ()

  let empty_box = Box.create ()
  let toggle_on box = Box.set_style box on_style
  let toggle_off box = Box.set_style box off_style
  let get_layout btn = btn.layout

  let make_btn (d : Date.t) m info =
    let label =
      W.label ~size
        ~fg:
          (if d.month = m then Draw.opaque Draw.black else Draw.opaque Draw.grey)
        ~align:Draw.Center (string_of_int d.day)
    in
    let box = W.box ~w ~h ~style:off_style () in
    let l_layout = L.resident label in
    let out =
      {
        layout = L.superpose [ L.resident box; l_layout ];
        label;
        box = W.get_box box;
        d;
      }
    in
    let _ =
      if d = info.date then (
        W.get_box box |> toggle_on;
        info.cur_btn <- W.get_box box)
      else ();
      W.on_click
        ~click:(fun _ ->
          let b = W.get_box box in
          if Box.get_style b = off_style then (
            toggle_off info.cur_btn;
            toggle_on b;
            info.cur_btn <- b;
            info.date <- out.d)
          else ();
          info.on_update d)
        label;
      Space.full_width l_layout
    in
    out

  let change_btn btn (d : Date.t) m info =
    Sync.push (fun () ->
        W.set_text btn.label (string_of_int d.day);
        Label.set_fg_color (W.get_label btn.label)
          (if d.month = m then Draw.opaque Draw.black else Draw.opaque Draw.grey);
        if d = info.date then (
          info.cur_btn <- btn.box;
          toggle_on btn.box)
        else toggle_off btn.box;
        btn.d <- d)
end

type t = {
  layout : L.t;
  m_lst : Btn.t list;
  m_label : W.t;
  info : sel_info;
  mutable cur_m : Date.month;
  mutable cur_y : int;
}

(** Gets a 6-week month. *)
let get_month m y =
  let last = Date.last_day m y in
  let rec last_week len = function
    | [] -> failwith "Empty list"
    | h :: t ->
        if Date.day_of_week h = Saturday && len + last.day > 35 then h :: t
        else last_week (len + 1) (Date.next_day h :: h :: t)
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
  last_week 0 [ last ] |> List.rev |> add_month |> fst_week

let get_month_lst info m y =
  let days = get_month m y in
  let rec helper = function
    | [] -> []
    | h :: t -> Btn.make_btn h m info :: helper t
  in
  helper days

(** Returns a layout of a month with the specified info. *)
let layout_of_month month_lst =
  let week_layout days =
    let rec helper = function
      | [] -> []
      | h :: t -> Btn.get_layout h :: helper t
    in
    helper days |> L.flat ~margins:0
  in
  let rec helper = function
    | [] -> []
    | d1 :: d2 :: d3 :: d4 :: d5 :: d6 :: d7 :: t ->
        week_layout [ d1; d2; d3; d4; d5; d6; d7 ] :: helper t
    | lst -> [ week_layout lst ]
  in
  helper month_lst |> L.tower ~margins:0

(** Sets the display of [sel] to the month and year specified. *)
let update_display sel =
  let days = get_month sel.cur_m sel.cur_y in
  let rec helper = function
    | [], [] -> ()
    | btn :: t1, d :: t2 ->
        let _ = Btn.change_btn btn d sel.cur_m sel.info in
        helper (t1, t2)
    | _ -> failwith "Should be same length"
  in
  helper (sel.m_lst, days);
  W.set_text sel.m_label
    (Date.string_of_month sel.cur_m ^ " " ^ string_of_int sel.cur_y)

let make_selector () =
  let info =
    {
      date = Date.current_date ();
      cur_btn = Btn.empty_box;
      on_update = (fun _ -> ());
    }
  in
  let m_lst = get_month_lst info info.date.month info.date.year in
  let m_layout = layout_of_month m_lst in
  let m_label =
    W.label ~align:Draw.Center
      (Date.string_of_month info.date.month ^ " " ^ string_of_int info.date.year)
  in
  let prev_btn =
    W.button ~kind:Button.Trigger
      ~label:(Label.create ~align:Draw.Center "<")
      "Prev btn"
  in
  let next_btn =
    W.button ~kind:Button.Trigger
      ~label:(Label.create ~align:Draw.Center ">")
      "Next\n     btn"
  in
  let top = L.flat_of_w [ prev_btn; m_label; next_btn ] in
  let stuff = L.tower [ top; m_layout ] in
  let box = surrounding_box stuff in
  let sel =
    {
      layout = L.superpose [ box; stuff ];
      m_lst;
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
        let m, y = Date.prev_month sel.cur_m sel.cur_y in
        sel.cur_m <- m;
        sel.cur_y <- y;
        update_display sel)
      prev_btn;
    W.on_button_release
      ~release:(fun _ ->
        let m, y = Date.next_month sel.cur_m sel.cur_y in
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
