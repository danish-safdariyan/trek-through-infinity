open Bogue
(** For general display functions. *)

module L = Layout
module W = Widget

(** Creates a box that surounds the layout; Default background is white, default
    border is dark_grey. *)
let surrounding_box ?(background = Draw.find_color "#00B9BC")
    ?(border_color = Draw.dark_grey) layout =
  W.box ~w:(L.width layout) ~h:(L.height layout)
    ~style:
      (Style.create
         ~background:(Draw.opaque background |> Style.color_bg)
         ~border:
           (Style.mk_line ~color:(Draw.opaque border_color) ~width:2 ()
           |> Style.mk_border)
         ())
    ()
  |> L.resident
