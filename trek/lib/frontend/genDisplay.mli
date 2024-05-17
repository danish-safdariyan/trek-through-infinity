open Bogue

val surrounding_box :
  ?background:Draw.rgb ->
  ?border_color:Draw.rgb ->
  ?width:int ->
  Layout.t ->
  Layout.t
(** Creates a box that surounds the layout; Default background is a shade of
    turquoise "#00B9BC", default border is dark_turquoise "#003738". *)

val theme_box : ?width:int -> int -> int -> Layout.t
(** Creates a box with the theme colors using the specified width and height. *)

val adjust_label_size : int -> Widget.t -> unit
(** [adjust_label_size max_size label] cuts off [label] if it is over
    [max_size]. *)
