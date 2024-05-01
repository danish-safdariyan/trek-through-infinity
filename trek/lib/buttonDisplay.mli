open Bogue

val button : int -> Widget.t

val next_btn :
  (unit -> Date.month * int) -> (Date.month * int -> unit) -> Widget.t
(** [next_btn get_month update_month] is a button that will move the calendar to
    the next month when pressed. *)

val prev_btn :
  (unit -> Date.month * int) -> (Date.month * int -> unit) -> Widget.t
(** [prev_btn get_month update_month] is a button that will move the calendar to
    the previous month when pressed. *)
