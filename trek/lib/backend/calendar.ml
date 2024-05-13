(* calendar.ml *)
(** Implementation of the Calendar module for managing events *)

module Map = CalDict.AssocListMap

let current_id = ref 20

type t = (Date.t, Event.t list) Map.t

let empty = Map.empty

(* Increment and get the next unique ID *)
let next_id () =
  incr current_id;
  !current_id

let add_days_to_date date_str days =
  let year, month, day =
    Scanf.sscanf date_str "%04d-%02d-%02d" (fun y m d -> (y, m, d))
  in
  let days_in_month = function
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | 4 | 6 | 9 | 11 -> 30
    | 2 ->
        if year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0) then 29
        else 28
    | _ -> failwith "Invalid month"
  in
  let rec add d m y days =
    let dim = days_in_month m in
    if d + days <= dim then (y, m, d + days)
    else if m = 12 then add 1 1 (y + 1) (days - (dim - d + 1))
    else add 1 (m + 1) y (days - (dim - d + 1))
  in
  let new_year, new_month, new_day = add day month year days in
  Printf.sprintf "%04d-%02d-%02d" new_year new_month new_day

let calculate_next_date current_date repeat =
  match repeat with
  | Event.NoRepeat ->
      current_date
      (* Simply return the current date as there is no repetition. *)
  | Event.Daily -> add_days_to_date current_date 1
  | Event.Weekly -> add_days_to_date current_date 7
  | Event.Monthly ->
      let year, month, day =
        Scanf.sscanf current_date "%04d-%02d-%02d" (fun y m d -> (y, m, d))
      in
      let new_month = if month = 12 then 1 else month + 1 in
      let new_year = if new_month = 1 then year + 1 else year in
      let adjusted_day =
        let days_in_new_month =
          match new_month with
          | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
          | 4 | 6 | 9 | 11 -> 30
          | 2 ->
              if
                new_year mod 4 = 0
                && (new_year mod 100 <> 0 || new_year mod 400 = 0)
              then 29
              else 28
          | _ -> failwith "Invalid month"
        in
        min day days_in_new_month
      in
      Printf.sprintf "%04d-%02d-%02d" new_year new_month adjusted_day
  | Event.Yearly ->
      let year, month, day =
        Scanf.sscanf current_date "%04d-%02d-%02d" (fun y m d -> (y, m, d))
      in
      Printf.sprintf "%04d-%02d-%02d" (year + 1) month day

let add_event calendar date event =
  let events = try Map.lookup date calendar with Not_found -> [] in
  Map.insert date (event :: events) calendar

(* Definition of the make_event function matching the specified type signature
   in calendar.mli *)

let make_event ~title ~description ~date ~repeats ~times ~calendar : t =
  let rec create_and_add_events current_date repeats count calendar =
    if count = 0 then calendar
    else
      let next_date = calculate_next_date current_date repeats in
      let id = next_id () in
      let new_event =
        Event.create ~id ~title ~description ~date:current_date ~repeats
      in
      let updated_calendar =
        add_event calendar (Date.parse_date current_date) new_event
      in
      create_and_add_events next_date repeats (count - 1) updated_calendar
  in
  match repeats with
  | Event.NoRepeat ->
      let id = next_id () in
      let single_event = Event.create ~id ~title ~description ~date ~repeats in
      add_event calendar (Date.parse_date date) single_event
  | Event.Daily -> create_and_add_events date Event.Daily times calendar
  | Event.Weekly -> create_and_add_events date Event.Weekly times calendar
  | Event.Monthly -> create_and_add_events date Event.Monthly times calendar
  | Event.Yearly -> create_and_add_events date Event.Yearly times calendar

let add_events calendar events =
  List.fold_left
    (fun cal (date, event) -> add_event cal date event)
    calendar events

(* Easter calculation *)
let easter year =
  let a = year mod 19 in
  let b = year / 100 in
  let c = year mod 100 in
  let d = b / 4 in
  let e = b mod 4 in
  let f = (b + 8) / 25 in
  let g = (b - f + 1) / 3 in
  let h = ((19 * a) + b - d - g + 15) mod 30 in
  let i = c / 4 in
  let k = c mod 4 in
  let l = (32 + (2 * e) + (2 * i) - h - k) mod 7 in
  let m = (a + (11 * h) + (22 * l)) / 451 in
  let month = (h + l - (7 * m) + 114) / 31 in
  let day = ((h + l - (7 * m) + 114) mod 31) + 1 in
  Date.create year (Date.int_to_month month) day

(* Initialize calendar with annual events *)
let initialize_calendar year =
  let fixed_dates =
    [
      ( Date.create year January 1,
        Event.create ~id:1 ~title:"New Year's Day"
          ~description:"Celebration of the new year"
          ~date:(Date.to_string_iso8601 (Date.create year January 1))
          ~repeats:Yearly );
      ( Date.nth_weekday_of_month January Monday 3 year,
        Event.create ~id:2 ~title:"Martin Luther King Jr. Day"
          ~description:"Celebration of Martin Luther King Jr."
          ~date:
            (Date.to_string_iso8601
               (Date.nth_weekday_of_month January Monday 3 year))
          ~repeats:Yearly );
      ( Date.create year February 14,
        Event.create ~id:3 ~title:"Valentine's Day"
          ~description:"Day of love and affection"
          ~date:(Date.to_string_iso8601 (Date.create year February 14))
          ~repeats:Yearly );
      ( Date.nth_weekday_of_month February Monday 3 year,
        Event.create ~id:4 ~title:"Presidents' Day"
          ~description:"Celebration of US Presidents"
          ~date:
            (Date.to_string_iso8601
               (Date.nth_weekday_of_month February Monday 3 year))
          ~repeats:Yearly );
      ( Date.create year March 17,
        Event.create ~id:5 ~title:"St. Patrick's Day"
          ~description:"Celebration of Irish culture"
          ~date:(Date.to_string_iso8601 (Date.create year March 17))
          ~repeats:Yearly );
      ( easter year,
        Event.create ~id:6 ~title:"Easter Sunday"
          ~description:"Christian holiday celebrating the resurrection of Jesus"
          ~date:(Date.to_string_iso8601 (easter year))
          ~repeats:Yearly );
      ( Date.create year April 22,
        Event.create ~id:7 ~title:"Earth Day"
          ~description:"Promotion of environmental awareness"
          ~date:(Date.to_string_iso8601 (Date.create year April 22))
          ~repeats:Yearly );
      ( Date.nth_weekday_of_month May Sunday 2 year,
        Event.create ~id:8 ~title:"Mother's Day"
          ~description:"Celebration of mothers"
          ~date:
            (Date.to_string_iso8601
               (Date.nth_weekday_of_month May Sunday 2 year))
          ~repeats:Yearly );
      ( Date.last_weekday_of_month May Monday year,
        Event.create ~id:9 ~title:"Memorial Day"
          ~description:"Honoring military personnel who have died"
          ~date:
            (Date.to_string_iso8601
               (Date.last_weekday_of_month May Monday year))
          ~repeats:Yearly );
      ( Date.nth_weekday_of_month June Sunday 3 year,
        Event.create ~id:10 ~title:"Father's Day"
          ~description:"Celebration of fathers"
          ~date:
            (Date.to_string_iso8601
               (Date.nth_weekday_of_month June Sunday 3 year))
          ~repeats:Yearly );
      ( Date.create year July 4,
        Event.create ~id:11 ~title:"Independence Day"
          ~description:"Celebration of American independence"
          ~date:(Date.to_string_iso8601 (Date.create year July 4))
          ~repeats:Yearly );
      ( Date.nth_weekday_of_month September Monday 1 year,
        Event.create ~id:12 ~title:"Labor Day" ~description:"Honoring workers"
          ~date:
            (Date.to_string_iso8601
               (Date.nth_weekday_of_month September Monday 1 year))
          ~repeats:Yearly );
      ( Date.nth_weekday_of_month October Monday 2 year,
        Event.create ~id:13 ~title:"Columbus Day"
          ~description:"Commemoration of Christopher Columbus's arrival"
          ~date:
            (Date.to_string_iso8601
               (Date.nth_weekday_of_month October Monday 2 year))
          ~repeats:Yearly );
      ( Date.create year October 31,
        Event.create ~id:14 ~title:"Halloween"
          ~description:"Celebration involving costumes and treats"
          ~date:(Date.to_string_iso8601 (Date.create year October 31))
          ~repeats:Yearly );
      ( Date.create year November 11,
        Event.create ~id:15 ~title:"Veterans Day"
          ~description:"Honoring military veterans"
          ~date:(Date.to_string_iso8601 (Date.create year November 11))
          ~repeats:Yearly );
      ( Date.nth_weekday_of_month November Thursday 4 year,
        Event.create ~id:16 ~title:"Thanksgiving Day"
          ~description:"Day for giving thanks and family gatherings"
          ~date:
            (Date.to_string_iso8601
               (Date.nth_weekday_of_month November Thursday 4 year))
          ~repeats:Yearly );
      ( Date.create year December 24,
        Event.create ~id:17 ~title:"Christmas Eve"
          ~description:"Day before Christmas"
          ~date:(Date.to_string_iso8601 (Date.create year December 24))
          ~repeats:Yearly );
      ( Date.create year December 25,
        Event.create ~id:18 ~title:"Christmas Day"
          ~description:"Celebration of the birth of Jesus Christ"
          ~date:(Date.to_string_iso8601 (Date.create year December 25))
          ~repeats:Yearly );
      ( Date.create year December 31,
        Event.create ~id:19 ~title:"New Year's Eve"
          ~description:"Celebration of the last day of the year"
          ~date:(Date.to_string_iso8601 (Date.create year December 31))
          ~repeats:Yearly );
    ]
  in
  add_events empty fixed_dates

let remove_event calendar date event_id =
  let events = try Map.lookup date calendar with Not_found -> [] in
  let filtered_events =
    List.filter (fun e -> Event.get_id e <> event_id) events
  in
  Map.insert date filtered_events calendar

let edit_event calendar date event_id updated_event =
  let events = try Map.lookup date calendar with Not_found -> [] in
  let updated_events =
    List.map
      (fun e -> if Event.get_id e = event_id then updated_event else e)
      events
  in
  Map.insert date updated_events calendar

let find_events calendar date =
  try Map.lookup date calendar with Not_found -> []

let list_all_events calendar =
  Map.bindings calendar
  |> List.map (fun (_, events) -> List.map Event.to_string events)
     (* Convert each event to string *)
  |> List.flatten
(* Flatten the list of lists of strings into a list of strings *)
