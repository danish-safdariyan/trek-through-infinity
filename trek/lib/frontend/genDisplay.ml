open Bogue
(** For general display functions. *)

module L = Layout
module W = Widget

(** Creates a box that surounds the layout; Default background is white, default
    border is dark_grey. *)
let surrounding_box ?background ?border_color layout =
  W.box ~w:(L.width layout) ~h:(L.height layout)
    ~style:
      (Style.create
         ~background:
           (Draw.opaque
              (match background with
              | None -> Draw.white
              | Some b -> b)
           |> Style.color_bg)
         ~border:
           (Style.mk_line
              ~color:
                (Draw.opaque
                   (match border_color with
                   | None -> Draw.dark_grey
                   | Some c -> c))
              ~width:2 ()
           |> Style.mk_border)
         ())
    ()
  |> L.resident
