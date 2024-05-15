open Bogue
(** For general display functions. *)

module L = Layout
module W = Widget

(** Creates a box that surounds the layout; Default background is a shade of
    turquoise "#00B9BC", default border is dark_turquoise "#003738". *)
let surrounding_box ?(background = Draw.find_color "#00B9BC")
    ?(border_color = Draw.find_color "#006f71") ?(width = 4) layout =
  W.box ~w:(L.width layout) ~h:(L.height layout)
    ~style:
      (Style.create
         ~background:(Draw.opaque background |> Style.color_bg)
         ~border:
           (Style.mk_line ~color:(Draw.opaque border_color) ~width ()
           |> Style.mk_border)
         ())
    ()
  |> L.resident

let theme_box w h =
  W.box ~w ~h
    ~style:
      (Style.create
         ~background:(Draw.opaque (Draw.find_color "#00B9BC") |> Style.color_bg)
         ~border:
           (Style.mk_line ~color:(Draw.opaque Draw.dark_grey) ~width:2 ()
           |> Style.mk_border)
         ())
    ()
  |> L.resident
