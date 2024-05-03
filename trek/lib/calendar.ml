(* calendar.ml *)
(** Implementation of the Calendar module for managing events *)

module Map = CalDict.AssocListMap

type t = (Date.t, Event.t list) Map.t

let empty = Map.empty

let add_event calendar date event =
  (*returns empty list if date is not found*)
  let events = try Map.lookup date calendar with Not_found -> [] in
  (*if date is found, add event to the list of events for that day*)
  Map.insert date (event :: events) calendar

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
  (* if day is not in calendar, return empty list *)
  let events = try Map.lookup date calendar with Not_found -> [] in
  let filtered_events =
    (* if the day is in the calendar, filter through the events and return the
       new list without the event id given *)
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
