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

let create year month day = { year; month; day }
let get_year date = date.year
let get_month date = date.month
let get_day date = date.day

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

let parse_date str =
  try
    Scanf.sscanf str "%d-%d-%d" (fun year month day ->
        create year
          (match month with
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
          | _ -> failwith "Invalid month value")
          day)
  with _ -> failwith "Invalid date format. Expected: YYYY-MM-DD"

let days_in_month year month =
  match month with
  | January | March | May | July | August | October | December -> 31
  | April | June | September | November -> 30
  | February ->
      if year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0) then 29
      else 28

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

  (* Correcting formula to match your day_of_week type with 0 as Sunday *)
  match h with
  | 0 -> Saturday (* Zeller's output for Saturday *)
  | 1 -> Sunday (* Zeller's output for Sunday *)
  | 2 -> Monday (* Zeller's output for Monday *)
  | 3 -> Tuesday (* Zeller's output for Tuesday *)
  | 4 -> Wednesday (* Zeller's output for Wednesday *)
  | 5 -> Thursday (* Zeller's output for Thursday *)
  | 6 -> Friday (* Zeller's output for Friday *)
  | _ -> failwith "Invalid day of week computation" (* Should never occur *)

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

let next_day { year; month; day } =
  if { year; month; day } = last_day month year then
    let m, y = next_month month year in
    create y m 1
  else create year month (day + 1)

let prev_day { year; month; day } =
  if day = 1 then
    let m, y = prev_month month year in
    last_day m y
  else create year month (day - 1)

(* more functions edited April 27th *)
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

(* useful for displaying dates in the UI or exporting data. *)
let format_date date =
  Printf.sprintf "%04d-%02d-%02d" date.year (int_of_month date.month) date.day

(* ******************************************************************* *)
(* useful for scheduling and event reminders. if implemented those
   functionalites *)

(* Correctly computes the number of days from an epoch assuming the epoch starts
   at 1 Jan 0001 *)

(* let day_of_year date = let days_in_months = [| 0; 31; 28; 31; 30; 31; 30; 31;
   31; 30; 31; 30; 31 |] in let is_leap year = (year mod 4 = 0 && year mod 100
   <> 0) || year mod 400 = 0 in let result = ref 0 in for i = 1 to int_of_month
   date.month - 1 do result := !result + days_in_months.(i); if i = 2 && is_leap
   date.year then result := !result + 1 done; !result + date.day

   let total_days_from_epoch date = let days_in_past_years = ((date.year - 1) *
   365) + ((date.year - 1) / 4) - ((date.year - 1) / 100) + ((date.year - 1) /
   400) in days_in_past_years + day_of_year date

   let date_difference date1 date2 = let days1 = total_days_from_epoch date1 in
   let days2 = total_days_from_epoch date2 in abs (days2 - days1) *)

(* ******************************************************************* *)

(* ******************************************************************* *)

(* Checks if a given date falls on a weekend. *)
let is_weekend date =
  match day_of_week date with
  | Saturday | Sunday -> true
  | _ -> false

(* leap year *)
let is_leap_year year = year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0)

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
  (* Calculate the offset to the first occurrence of the target weekday *)
  let offset =
    (int_of_day_of_week weekday - int_of_day_of_week first_weekday_of_month + 7)
    mod 7
  in
  let day = 1 + offset + ((n - 1) * 7) in
  if day > days_in_month year month then
    failwith "Requested nth weekday exceeds month length"
  else create year month day

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

let last_weekday_of_month month weekday year =
  let rec find_last_weekday current_day =
    if day_of_week { year; month; day = current_day } = weekday then
      create year month current_day
    else find_last_weekday (current_day - 1)
  in
  let last_day = days_in_month year month in
  find_last_weekday last_day

let to_string_iso8601 date =
  Printf.sprintf "%04d-%02d-%02d" date.year (int_of_month date.month) date.day

let to_string date =
  Printf.sprintf "%04d-%02d-%02d" date.year (int_of_month date.month) date.day
