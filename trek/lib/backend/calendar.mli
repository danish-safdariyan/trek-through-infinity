(** Implementation of the Calendar module for managing events, calendar.mli*)

type t
(** Type representing a calendar *)

val empty : t
(** [empty] is an empty calendar *)

val make_event :
  string -> string -> Event.repeat_option -> Event.color_choices -> Event.t
(** [make_event title description] creates an event with the provided
    information. Should use this function over [Event.create]. *)

val add_event :
  Date.t ->
  string ->
  string ->
  Event.repeat_option ->
  Event.color_choices ->
  t ->
  t
(** [add_event date title description repeats calendar] makes a new event with
    the given information and adds it to the calendar on the specified date. *)

val add_existing_event : Date.t -> Event.t -> t -> t
(** [add_event date event calendar] adds the pre-existing event to the calendar
    on the specified date *)

val easter : int -> Date.t

val initialize_calendar : t -> t
(** [initialize_calendar calendar] initializes the calendar with annual events
    for the given year *)

val remove_event : Date.t -> Event.t -> t -> t
(** [remove_event date event calendar] removes [event] which is on [date]. If it
    is a repeating event, removes all events. *)

val edit_event : Date.t -> Event.t -> Event.t -> t -> t
(** [edit_event date event updated_event calendar] replaces [event] on [date]
    with [updated_event]. If it is a repeating event, all events will be
    updated. *)

val find_events : t -> Date.t -> Event.t list
(** [find_events calendar date] finds events on the specified date *)

val list_all_events : t -> string list
(** [list_all_events calendar] lists all events in the calendar *)
