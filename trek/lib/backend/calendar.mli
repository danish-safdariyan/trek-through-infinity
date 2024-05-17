(* calendar.mli *)

module Map = CalDict.AssocListMap

type day = int * Date.month
(** Type alias for a day, represented as a tuple of day and month. *)

type t = {
  one_time : (Date.t, Event.t list) Map.t;
  daily : Event.t list;
  weekly : Event.t list array;
  monthly : Event.t list array;
  yearly : (day, Event.t list) Map.t;
}
(** Type representing the calendar, containing various event lists. *)

val empty : t
(** [empty] is the empty calendar instance. *)

val next_id : unit -> int
(** [next_id ()] increments and returns the next unique ID. *)

val make_event :
  string -> string -> Event.repeat_option -> Event.color_choices -> Event.t
(** [make_event title description repeats color] creates an event with the given
    parameters. *)

val add_existing_event : Date.t -> Event.t -> t -> t
(** [add_existing_event date event calendar] adds an existing event to the
    calendar on the specified date. *)

val add_event :
  Date.t ->
  string ->
  string ->
  Event.repeat_option ->
  Event.color_choices ->
  t ->
  t
(** [add_event date title description repeats color calendar] creates and adds a
    new event to the calendar on the specified date. *)

val easter : int -> Date.t
(** [easter year] calculates the date of Easter for the given year. *)

val initialize_calendar : t -> t
(** [initialize_calendar cal] initializes the calendar with annual events. *)

val remove_event : Date.t -> Event.t -> t -> t
(** [remove_event date event calendar] removes an event from the calendar on the
    specified date. *)

val edit_event : Date.t -> Event.t -> Event.t -> t -> t
(** [edit_event date event updated_event calendar] edits an event in the
    calendar on the specified date. *)

val find_events : t -> Date.t -> Event.t list
(** [find_events calendar date] finds all events on the given date. *)

val list_all_events : t -> string list
(** [list_all_events calendar] lists all events in the calendar. *)
