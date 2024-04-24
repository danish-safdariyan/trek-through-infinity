module Date = struct
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
      | _ -> failwith "Invalid month value"
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
end
