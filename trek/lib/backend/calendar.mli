(** Implementation of the Calendar module for managing events *)

type t
(** Type representing a calendar *)

val empty : t
(** [empty] is an empty calendar *)

val make_event :
  title:string ->
  description:string ->
  date:string ->
  repeats:Event.repeat_option ->
  Event.t
(** [make_event ~title ~description ~date ~repeats] creates a new event with the
    given parameters *)

val add_event : t -> Date.t -> Event.t -> t
(** [add_event calendar date event] adds the given event to the calendar on the
    specified date *)

val add_events : t -> (Date.t * Event.t) list -> t
(** [add_events calendar events] adds multiple events to the calendar *)

val easter : int -> Date.t

val initialize_calendar : int -> t
(** [initialize_calendar year] initializes the calendar with annual events for
    the given year *)

val remove_event : t -> Date.t -> int -> t
(** [remove_event calendar date event_id] removes the event with the given ID
    from the calendar on the specified date *)

val edit_event : t -> Date.t -> int -> Event.t -> t
(** [edit_event calendar date event_id updated_event] updates the event with the
    given ID on the specified date with the updated event *)

val find_events : t -> Date.t -> Event.t list
(** [find_events calendar date] finds events on the specified date *)

val list_all_events : t -> string list
(** [list_all_events calendar] lists all events in the calendar *)
