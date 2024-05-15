open Backend
open Bogue

val add_event_layout :
  (Date.t -> string -> string -> Event.repeat_option -> unit) -> Layout.t
(** [add_event_layout add_event] returns a layout with the functionality to add
    an event by calling the [add_event] function. *)
