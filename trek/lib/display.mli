val test : unit -> unit

val update_calendar : Calendar.t -> unit
(** Updates the calendar shown in display; also updates the display. *)

val update_display : unit -> unit
(** Updates the display when the screen next refreshes. You most likely will not
    need to call this function. *)
