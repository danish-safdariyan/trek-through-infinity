(* date.ml *)

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

(* Creates a date with given year, month, and day *)
let create year month day = { year; month; day }

(* Returns the year from a date *)
let get_year date = date.year

(* Returns the month from a date *)
let get_month date = date.month

(* Returns the day from a date *)
let get_day date = date.day

(* Returns the current system date *)
let current_date () =
  let now = Unix.localtime (Unix.time ()) in
  {
    year = now.tm_year + 1900;
    month =
      (match now.tm_mon with
      | 0 -> January
      | 1 -> February
      | 2 -> March
      | 3 -> April
      | 4 -> May
      | 5 -> June
      | 6 -> July
      | 7 -> August
      | 8 -> September
      | 9 -> October
      | 10 -> November
      | _ -> December);
    day = now.tm_mday;
  }

(* Returns the number of days in a given month and year *)
let days_in_month year month =
  match month with
  | January | March | May | July | August | October | December -> 31
  | April | June | September | November -> 30
  | February ->
      if year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0) then 29
      else 28

(* Determines the day of the week for a given date using Zeller's Congruence *)
let day_of_week date =
  let { year; month; day } = date in
  let m =
    match month with
    | January -> 13
    | February -> 14
    | March -> 3
    | April -> 4
    | May -> 5
    | June -> 6
    | July -> 7
    | August -> 8
    | September -> 9
    | October -> 10
    | November -> 11
    | December -> 12
  in
  let y = if m > 12 then year - 1 else year in
  (* Adjust year for January and February *)
  let k = y mod 100 in
  let j = y / 100 in
  (* Century component of the year *)
  let h = (day + ((m + 1) * 26 / 10) + k + (k / 4) + (j / 4) + (5 * j)) mod 7 in
  match h with
  | 0 -> Saturday
  | 1 -> Sunday
  | 2 -> Monday
  | 3 -> Tuesday
  | 4 -> Wednesday
  | 5 -> Thursday
  | 6 -> Friday
  | _ -> failwith "Invalid day of week computation" (* Should never occur *)

(* Returns the last day of a given month and year *)
let last_day m y =
  let d =
    match m with
    | January -> 31
    | February ->
        if (y mod 4 = 0 && y mod 100 != 0) || y mod 400 = 0 then 29 else 28
    | March -> 31
    | April -> 30
    | May -> 31
    | June -> 30
    | July -> 31
    | August -> 31
    | September -> 30
    | October -> 31
    | November -> 30
    | December -> 31
  in
  create y m d

(* Returns the next month following a given month and year *)
let next_month m y =
  match m with
  | January -> (February, y)
  | February -> (March, y)
  | March -> (April, y)
  | April -> (May, y)
  | May -> (June, y)
  | June -> (July, y)
  | July -> (August, y)
  | August -> (September, y)
  | September -> (October, y)
  | October -> (November, y)
  | November -> (December, y)
  | December -> (January, y + 1)

(* Returns the previous month before a given month and year *)
let prev_month m y =
  match m with
  | January -> (December, y - 1)
  | February -> (January, y)
  | March -> (February, y)
  | April -> (March, y)
  | May -> (April, y)
  | June -> (May, y)
  | July -> (June, y)
  | August -> (July, y)
  | September -> (August, y)
  | October -> (September, y)
  | November -> (October, y)
  | December -> (November, y)

(* Returns the next day after a given date *)
let next_day { year; month; day } =
  if { year; month; day } = last_day month year then
    let m, y = next_month month year in
    create y m 1
  else create year month (day + 1)

(* Returns the day before a given date *)
let prev_day { year; month; day } =
  if day = 1 then
    let m, y = prev_month month year in
    last_day m y
  else create year month (day - 1)

(* Converts an integer to a month *)
let int_of_month month =
  match month with
  | January -> 1
  | February -> 2
  | March -> 3
  | April -> 4
  | May -> 5
  | June -> 6
  | July -> 7
  | August -> 8
  | September -> 9
  | October -> 10
  | November -> 11
  | December -> 12

(* Formats a date to a string in YYYY-MM-DD format *)
let format_date date =
  Printf.sprintf "%04d-%02d-%02d" date.year (int_of_month date.month) date.day

(* Converts a month to an integer *)
let int_of_month month =
  match month with
  | January -> 1
  | February -> 2
  | March -> 3
  | April -> 4
  | May -> 5
  | June -> 6
  | July -> 7
  | August -> 8
  | September -> 9
  | October -> 10
  | November -> 11
  | December -> 12

(* Converts a month to an string *)
let string_of_month = function
  | January -> "January "
  | February -> "February "
  | March -> "March "
  | April -> "April "
  | May -> "May "
  | June -> "June "
  | July -> "July "
  | August -> "August "
  | September -> "September "
  | October -> "October "
  | November -> "November "
  | December -> "December "

(* Determines if a given date is a weekend *)
let is_weekend date =
  match day_of_week date with
  | Saturday | Sunday -> true
  | _ -> false

(* Determines if a given year is a leap year *)
let is_leap_year year = year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0)

(* Converts a day_of_week type to an integer *)
let int_of_day_of_week = function
  | Sunday -> 0
  | Monday -> 1
  | Tuesday -> 2
  | Wednesday -> 3
  | Thursday -> 4
  | Friday -> 5
  | Saturday -> 6

(* Returns the date of the nth occurrence of a given weekday in a specific month
   and year *)
let nth_weekday_of_month month weekday n year =
  let first_of_month = create year month 1 in
  let first_weekday_of_month = day_of_week first_of_month in
  let offset =
    (int_of_day_of_week weekday - int_of_day_of_week first_weekday_of_month + 7)
    mod 7
  in
  let day = 1 + offset + ((n - 1) * 7) in
  if day > days_in_month year month then
    failwith "Requested nth weekday exceeds month length"
  else create year month day

(* Converts an integer to a month *)
let int_to_month = function
  | 1 -> January
  | 2 -> February
  | 3 -> March
  | 4 -> April
  | 5 -> May
  | 6 -> June
  | 7 -> July
  | 8 -> August
  | 9 -> September
  | 10 -> October
  | 11 -> November
  | 12 -> December
  | _ -> failwith "Invalid month value"

(* Finds the last occurrence of a specific weekday in a given month and year *)
let last_weekday_of_month month weekday year =
  let rec find_last_weekday current_day =
    if day_of_week { year; month; day = current_day } = weekday then
      create year month current_day
    else find_last_weekday (current_day - 1)
  in
  let last_day = days_in_month year month in
  find_last_weekday last_day

(* Converts a date to a string in YYYY-MM-DD format *)
let to_string date =
  Printf.sprintf "%04d-%02d-%02d" date.year (int_of_month date.month) date.day

(* Compares two dates, returning a positive number if the first date is later
   than the second, zero if they are equal, and a negative number otherwise *)
let compare d1 d2 =
  if d1.year - d2.year <> 0 then d1.year - d2.year
  else
    let temp = int_of_month d1.month - int_of_month d2.month in
    if temp <> 0 then temp else d1.day - d2.day
