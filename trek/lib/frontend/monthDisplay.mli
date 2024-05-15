open Backend
open Bogue

type t
(** [t] represents a month. *)

val get_month : Date.month -> int -> t
(** [get_month month year] is the month of [month] in [year]. *)

val layout_of_month :
  int -> Calendar.t -> t -> ((Calendar.t -> Calendar.t) -> unit) -> Layout.t
(** [layout_of_month w cal month update_calendar] is a Bogue layout with the
    days of the month and their tasks. [w] is the width of each day (may remove
    later), [cal] the calendar to be printed. *)

val get_month_info : t -> Date.month * int
(** Gives the month and year the input corresponds to. *)

val string_of_month : t -> string
(** The month in string format. *)
