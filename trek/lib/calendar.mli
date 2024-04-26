(* calendar.mli *)
(** Interface for the Calendar module, managing a collection of events *)

type t
(** The type representing the entire calendar *)

val empty : t
(** Returns an empty calendar *)

val add_event : t -> Date.t -> Event.t -> t
(** [add_event calendar date event] adds an event to a specific date in the
    calendar *)

val remove_event : t -> Date.t -> int -> t
(** [remove_event calendar date event_id] removes an event by its ID from a
    specific date in the calendar *)

val edit_event : t -> Date.t -> int -> Event.t -> t
(** [edit_event calendar date event_id updated_event] edits an existing event in
    the calendar *)

val find_events : t -> Date.t -> Event.t list
(** [find_events calendar date] finds all events for a specific date *)

val list_all_events : t -> string list
(** [list_all_events calendar] lists all events in the calendar in string format *)
