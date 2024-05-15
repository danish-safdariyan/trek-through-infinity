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

val make_nth_weekday_of_month_event :
  Date.month -> Date.day_of_week -> int -> Event.t -> Event.t
(** [make_nth_weekday_of_month_event month weekday n] adds a condition for the
    nth weekday of a month to the given event. *)

val add_forever_events : t -> (Date.t * Event.t) list -> t
(** [add_forever_events calendar events] adds a list of forever repeating events
    to the calendar. *)

val initialize_calendar : t -> t
(** [initialize_calendar cal] initializes the calendar with annual events. *)

val remove_once_event : Date.t -> Event.t -> t -> t
(** [remove_once_event date event calendar] removes a one-time event from the
    calendar on the specified date. *)

val remove_daily_event : Event.t -> t -> t
(** [remove_daily_event event calendar] removes a daily event from the calendar. *)

val remove_weekly_event : Date.t -> Event.t -> t -> t
(** [remove_weekly_event date event calendar] removes a weekly event from the
    calendar on the specified date. *)

val remove_monthly_event : Date.t -> Event.t -> t -> t
(** [remove_monthly_event date event calendar] removes a monthly event from the
    calendar on the specified date. *)

val remove_yearly_event : Date.t -> Event.t -> t -> t
(** [remove_yearly_event date event calendar] removes a yearly event from the
    calendar on the specified date. *)

val remove_event : Date.t -> Event.t -> t -> t
(** [remove_event date event calendar] removes an event from the calendar on the
    specified date. *)

val edit_once_event : Date.t -> Event.t -> Event.t -> t -> t
(** [edit_once_event date event updated_event calendar] edits a one-time event
    in the calendar on the specified date. *)

val edit_daily_event : Event.t -> Event.t -> t -> t
(** [edit_daily_event event updated_event calendar] edits a daily event in the
    calendar. *)

val edit_weekly_event : Date.t -> Event.t -> Event.t -> t -> t
(** [edit_weekly_event date event updated_event calendar] edits a weekly event
    in the calendar on the specified date. *)

val edit_montly_event : Date.t -> Event.t -> Event.t -> t -> t
(** [edit_montly_event date event updated_event calendar] edits a monthly event
    in the calendar on the specified date. *)

val edit_yearly_event : Date.t -> Event.t -> Event.t -> t -> t
(** [edit_yearly_event date event updated_event calendar] edits a yearly event
    in the calendar on the specified date. *)

val edit_event : Date.t -> Event.t -> Event.t -> t -> t
(** [edit_event date event updated_event calendar] edits an event in the
    calendar on the specified date. *)

val find_once_events : Date.t -> t -> Event.t list
(** [find_once_events date calendar] finds all one-time events on the given
    date. *)

val find_weekly_events : Date.t -> t -> Event.t list
(** [find_weekly_events date calendar] finds all weekly events on the given
    date. *)

val find_monthly_events : Date.t -> t -> Event.t list
(** [find_monthly_events date calendar] finds all monthly events on the given
    date. *)

val find_yearly_events : Date.t -> t -> Event.t list
(** [find_yearly_events date calendar] finds all yearly events on the given
    date. *)

val find_events : t -> Date.t -> Event.t list
(** [find_events calendar date] finds all events on the given date. *)

val list_once_events : t -> string list
(** [list_once_events calendar] lists all one-time events in the calendar. *)

val list_daily_events : t -> string list
(** [list_daily_events calendar] lists all daily events in the calendar. *)

val list_weekly_events : t -> string list
(** [list_weekly_events calendar] lists all weekly events in the calendar. *)

val list_monthly_events : t -> string list
(** [list_monthly_events calendar] lists all monthly events in the calendar. *)

val list_yearly_events : t -> string list
(** [list_yearly_events calendar] lists all yearly events in the calendar. *)

val list_all_events : t -> string list
(** [list_all_events calendar] lists all events in the calendar. *)
