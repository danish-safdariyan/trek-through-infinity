type t
(** [t] represents a month. *)

val get_month : Backend.Date.month -> int -> t
(** [get_month month year] is the month of [month] in [year]. *)

val layout_of_month : int -> Backend.Calendar.t -> t -> Bogue.Layout.t
(** [layout_of_month w lst] is a Bogue layout with the days of the month and
    their tasks. [w] is the width of each day (may remove later), [lst] is a
    [(a * b) list] where [a] is the date and [b] is a list of all tasks for that
    day. *)

val get_month_info : t -> Backend.Date.month * int
(** Gives the month and year the input corresponds to. *)

val string_of_month : t -> string
(** The month in string format. *)
