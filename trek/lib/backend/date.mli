(** [month] Type representing months of the year. *)
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

(** [day_of_week] Type representing days of the week. *)
type day_of_week =
  | Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday

type t = {
  year : int;  (** Year component of the date. *)
  month : month;  (** Month component of the date. *)
  day : int;  (** Day component of the date. *)
}
(** [t] Type representing a date with year, month, and day components. *)

val create : int -> month -> int -> t
(** [create year month day] Creates a new date from year, month, and day. *)

val get_year : t -> int
(** [get_year date] Returns the year component of a date. *)

val get_month : t -> month
(** [get_month date] Returns the month component of a date. *)

val get_day : t -> int
(** [get_day date] Returns the day component of a date. *)

val current_date : unit -> t
(** [current_date ()] Returns the current date. *)

val parse_date : string -> t
(** [parse_date str] Parses a date string in "YYYY-MM-DD" format into a date
    type. *)

val days_in_month : int -> month -> int
(** [days_in_month year month] Returns the number of days in a given month and
    year. *)

val day_of_week : t -> day_of_week
(** [day_of_week date] Determines the day of the week for a given date. *)

val last_day : month -> int -> t
(** [last_day month year] Returns the last day of a given month and year as a
    date. *)

val next_month : month -> int -> month * int
(** [next_month month year] Calculates the next month and adjusts the year if
    necessary. *)

val prev_month : month -> int -> month * int
(** [prev_month month year] Calculates the previous month and adjusts the year
    if necessary. *)

val next_day : t -> t
(** [next_day date] Returns the next day, advancing to the next month or year if
    necessary. *)

val prev_day : t -> t
(** [prev_day date] Returns the previous day, moving back to the previous month
    or year if necessary. *)

val int_of_month : month -> int
(** [int_of_month month] Converts a month type to its corresponding integer
    representation. *)

val string_of_month : month -> string
(** [string_of_month month] converts a month type to the corresponding string. *)

val format_date : t -> string
(** [format_date date] Formats a date into a string in "YYYY-MM-DD" format. *)

val is_weekend : t -> bool
(** [is_weekend date] Checks if a given date falls on a weekend. *)

val is_leap_year : int -> bool
(** [is_leap_year year] Determines if a given year is a leap year. *)

val compare : t -> t -> int
(** [compare d1 d2] returns a value greater than 0 if [d1] comes after [d2]. It
    returns a value less than 0 if [d1] comes before [d2], and return 0 if [d1]
    and [d2] are the same. *)

val int_of_day_of_week : day_of_week -> int
val nth_weekday_of_month : month -> day_of_week -> int -> int -> t
val int_to_month : int -> month
val last_weekday_of_month : month -> day_of_week -> int -> t
val to_string_iso8601 : t -> string
val to_string : t -> string
