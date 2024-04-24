val layout_of_month : int -> (string * string list) list -> Bogue.Layout.t
(** [layout_of_month w lst] is a Bogue layout with the days of the month and
    their tasks. [w] is the width of each day (may remove later), [lst] is a
    [(a * b) list] where [a] is the date and [b] is a list of all tasks for that
    day. *)
