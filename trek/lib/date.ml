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
  let y = if month = January || month = February then year - 1 else year in
  let m =
    match month with
    | January | October -> 10
    | February | March -> 11
    | April | July -> 14
    | May -> 15
    | June -> 16
    | August -> 18
    | September | December -> 19
    | November -> 20 (* Added November *)
  in

  let d = day in
  let c = y / 100 in
  let y' = y mod 100 in
  let w =
    (y' + (y' / 4) + (c / 4) - (2 * c) + (26 * (m + 1) / 10) + d - 1) mod 7
  in
  match w with
  | 0 -> Sunday
  | 1 -> Monday
  | 2 -> Tuesday
  | 3 -> Wednesday
  | 4 -> Thursday
  | 5 -> Friday
  | _ -> Saturday

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
