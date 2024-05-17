open Bogue
(** For general display functions. *)

module L = Layout
module W = Widget

(** Creates a box that surounds the layout; Default background is a shade of
    turquoise "#00B9BC", default border is dark_turquoise "#003738". *)
let surrounding_box ?(background = Draw.find_color "#00B9BC")
    ?(border_color = Draw.find_color "#006f71") ?(width = 4) layout =
  let out =
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
  in
  out

(** Creates a box with the theme colors using the specified width and height. *)
let theme_box ?(width = 4) w h =
  W.box ~w ~h
    ~style:
      (Style.create
         ~background:(Draw.opaque (Draw.find_color "#00B9BC") |> Style.color_bg)
         ~border:
           (Style.mk_line
              ~color:(Draw.find_color "#006f71" |> Draw.opaque)
              ~width ()
           |> Style.mk_border)
         ())
    ()
  |> L.resident

(** Cuts off [label] if it is over [max_size] *)
let adjust_label_size max_size label =
  let temp = W.get_label label in
  let text = W.get_text label in
  if fst (Label.size temp) > max_size then (
    Label.set temp (text ^ "...");
    while
      fst (Label.size temp) > max_size && String.length (W.get_text label) > 1
    do
      let text = W.get_text label in
      if String.length text < 4 then Label.set temp " "
      else Label.set temp (String.sub text 0 (String.length text - 4) ^ "...")
    done)
