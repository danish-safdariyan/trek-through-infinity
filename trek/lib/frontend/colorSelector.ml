open Backend
open Bogue
module W = Widget
module L = Layout

(** Custom color buttons. *)
module type BtnSig = sig
  type t

  type info = {
    mutable cur_btn : t;
    mutable cur_color : Event.color_choices;
  }
  (** Info for color selector *)

  val empty : t
  (** Empty button. *)

  val get_btn : t -> W.t
  (** Gets widget button. *)

  val toggle_off : t -> unit
  (** Toggles off button. *)

  val toggle_on : t -> unit
  (** Toggles on button. *)

  val make_color_btn : info -> Event.color_choices -> t
  (** Creates a color button with the given info and color. *)
end

module Btn : BtnSig = struct
  type t = W.t * Style.background

  type info = {
    mutable cur_btn : t;
    mutable cur_color : Event.color_choices;
  }

  let empty = (W.empty ~w:0 ~h:0 (), Style.theme_bg)
  let size = 20
  let get_btn = fst

  let off_border =
    Style.mk_line ~color:(Draw.opaque Draw.black) ~width:1 ()
    |> Style.mk_border ~radius:(size / 2)

  let on_border =
    Style.mk_line ~color:(Draw.opaque Draw.pale_grey) ~width:3 ()
    |> Style.mk_border ~radius:(size / 2)

  let toggle_off btn =
    let b = W.get_box (fst btn) in
    Box.set_style b (Style.create ~background:(snd btn) ~border:off_border ())

  let toggle_on btn =
    let b = W.get_box (fst btn) in
    Box.set_style b (Style.create ~background:(snd btn) ~border:on_border ())

  let make_color_btn info color =
    let c =
      match color with
      | Event.Red -> Draw.red
      | Orange -> Draw.find_color "#C57106"
      | Yellow -> Draw.find_color "#CFAF07"
      | Green -> Draw.find_color "#08542A"
      | Blue -> Draw.blue
      | Indigo -> Draw.find_color "#00796E"
      | Violet -> Draw.find_color "#540853"
    in
    let background = Style.opaque_bg c in
    let out =
      ( W.box ~w:size ~h:size
          ~style:(Style.create ~background ~border:off_border ())
          (),
        background )
    in
    let _ =
      if info.cur_color = color then (
        info.cur_btn <- out;
        toggle_on out);
      W.on_click
        ~click:(fun w ->
          toggle_off info.cur_btn;
          toggle_on out;
          info.cur_color <- color;
          info.cur_btn <- out)
        (get_btn out)
    in
    out
end

type t = {
  layout : L.t;
  info : Btn.info;
  buttons : Btn.t list;
}

let get_color sel = sel.info.cur_color
let get_layout sel = sel.layout

let make_selector () =
  let colors = [ Event.Red; Orange; Yellow; Green; Blue; Indigo; Violet ] in
  let info = { Btn.cur_btn = Btn.empty; cur_color = Red } in
  let buttons = List.map (Btn.make_color_btn info) colors in
  let btn_layout = L.flat_of_w ~sep:10 (List.map Btn.get_btn buttons) in
  let layout =
    L.flat ~scale_content:false [ Space.hfill (); btn_layout; Space.hfill () ]
  in
  let _ = Space.full_width layout in
  { layout; info; buttons }

let set_color color sel =
  sel.info.cur_color <- color;
  Btn.toggle_off sel.info.cur_btn;
  let btn =
    List.nth sel.buttons
      (match color with
      | Event.Red -> 0
      | Orange -> 1
      | Yellow -> 2
      | Green -> 3
      | Blue -> 4
      | Indigo -> 5
      | Violet -> 6)
  in
  Btn.toggle_on btn;
  sel.info.cur_btn <- btn
