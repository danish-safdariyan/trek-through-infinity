type month =
  | January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December

type day_of_week =
  | Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday

type t = {
  year : int;
  month : month;
  day : int;
}

val create : int -> month -> int -> t
(** [create year month day] creates a date with the given [year], [month], and
    [day]. *)

val get_year : t -> int
(** [get_year date] returns the year of the given [date]. *)

val get_month : t -> month
(** [get_month date] returns the month of the given [date]. *)

val get_day : t -> int
(** [get_day date] returns the day of the given [date]. *)

val current_date : unit -> t
(** [current_date ()] returns the current date. *)

val parse_date : string -> t
(** [parse_date str] parses a date string in the format "YYYY-MM-DD" into a [t]
    structure. *)

val days_in_month : int -> month -> int
(** [days_in_month year month] calculates the number of days in a given [month]
    of a given [year]. *)

val day_of_week : t -> day_of_week
(** [day_of_week date] determines the day of the week for a given [date]. *)

val next_day : t -> t
(** [next_day date] is the day that comes after [date]. *)

val prev_day : t -> t
(** [prev_day date] is the day that comes before [date]. *)

val next_month : month -> int -> month * int
(** [next_month m y] is [(m', y')] where [m'] the month that comes after [m] and
    [y'] is the year of the month. *)

val prev_month : month -> int -> month * int
(** [prev_month m y] is [(m', y')] where [m'] the month that comes before [m]
    and [y'] is the year of the month. *)

val last_day : month -> int -> t
(** [last_day m y] is the last day of the month [m] in year [y]. *)
