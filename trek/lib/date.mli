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

(* ****** fix Later ******* *)
(* val day_of_year : t -> int (** [day_of_year date] Calculates the day of the
   year for a given date. *)

   val total_days_from_epoch : t -> int (** [total_days_from_epoch date]
   Calculates the total number of days from the Unix epoch to the given date. *)

   val date_difference : t -> t -> int (** [date_difference date1 date2]
   Calculates the absolute difference in days between two dates. *) *)
