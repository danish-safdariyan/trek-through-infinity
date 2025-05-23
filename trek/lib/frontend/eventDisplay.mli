open Backend
open Bogue

val layout_of_event :
  int ->
  Date.t ->
  Event.t ->
  Layout.t ->
  ((Calendar.t -> Calendar.t) -> unit) ->
  Layout.t
(** [layout_of_event w date event layout update_calendar] returns a layout of a
    [event]. [event] is taken from the calendar and [w] is passed down and
    depends on the current window size. Ultimately returns layout of task. *)

val add_event_layout : ((Calendar.t -> Calendar.t) -> unit) -> Layout.t
(** [add_event_layout update_calendar] returns a layout with the functionality
    to add an event by calling the [update_calendar] function. *)
