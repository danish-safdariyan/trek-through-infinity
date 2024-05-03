(* calendar.mli *)
(** Interface for the Calendar module for managing events *)

module Map = CalDict.AssocListMap

type t = (Date.t, Event.t list) Map.t
(** Type representing a calendar, mapping dates to lists of events *)

val empty : t
(** An empty calendar *)

val add_event : t -> Date.t -> Event.t -> t
(** [add_event calendar date event] adds an event to the calendar on the
    specified date. If the date already has events, the new event is appended to
    the existing list of events. *)

val add_events : t -> (Date.t * Event.t) list -> t
(** [add_events calendar events] adds multiple events to the calendar. The
    events should be provided as a list of tuples, where each tuple contains a
    date and an event. *)

val easter : int -> Date.t
(** [easter year] calculates the date of Easter Sunday for the given year. The
    calculation uses the "Computus" algorithm. *)

val initialize_calendar : int -> (Date.t, Event.t list) Map.t
(** [initialize_calendar year] initializes the calendar with fixed annual events
    for the specified year. *)

val remove_event : t -> Date.t -> int -> t
(** [remove_event calendar date event_id] removes the event with the specified
    ID from the calendar on the given date. If no event with the given ID is
    found, the calendar remains unchanged. *)

val edit_event : t -> Date.t -> int -> Event.t -> t
(** [edit_event calendar date event_id updated_event] updates the event with the
    specified ID on the given date with the provided updated event. If no event
    with the given ID is found, the calendar remains unchanged. *)

val find_events : t -> Date.t -> Event.t list
(** [find_events calendar date] finds and returns the list of events on the
    specified date. If no events are found, an empty list is returned. *)

val list_all_events : t -> string list
(** [list_all_events calendar] lists all events in the calendar. The events are
    returned as a list of strings, each representing an event. *)
