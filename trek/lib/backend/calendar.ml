(* calendar.ml *)
(** Implementation of the Calendar module for managing events *)

module Map = CalDict.AssocListMap

type repeat_option =
  | NoRepeat
  | Daily
  | Weekly
  | Monthly
  | Yearly

let current_id = ref 20

type day = int * Date.month

let get_day (d : Date.t) : day = (d.day, d.month)

type t = {
  one_time : (Date.t, Event.t list) Map.t;
  daily : Event.t list;
  weekly : Event.t list array;
  monthly : Event.t list array;
  yearly : (day, Event.t list) Map.t;
}

let empty =
  {
    one_time = Map.empty;
    daily = [];
    weekly = Array.make 7 [];
    monthly = Array.make 31 [];
    yearly = Map.empty;
  }

(* Increment and get the next unique ID *)
let next_id () =
  incr current_id;
  !current_id

(* Create an event *)
let make_event title description =
  let id = next_id () in
  Event.create ~id ~title ~description

let add_existing_event date event repeats calendar =
  match repeats with
  | NoRepeat ->
      let events =
        try Map.lookup date calendar.one_time with Not_found -> []
      in
      {
        calendar with
        one_time = Map.insert date (event :: events) calendar.one_time;
      }
  | Daily -> { calendar with daily = event :: calendar.daily }
  | Weekly ->
      let weekday = Date.day_of_week date |> Date.int_of_day_of_week in
      let arr = Array.copy calendar.weekly in
      let _ = arr.(weekday) <- event :: Array.get arr weekday in
      { calendar with weekly = arr }
  | Monthly ->
      let d = date.day - 1 in
      let arr = Array.copy calendar.monthly in
      let _ = arr.(d) <- event :: Array.get arr d in
      { calendar with monthly = arr }
  | Yearly ->
      let events =
        try Map.lookup (date.day, date.month) calendar.yearly
        with Not_found -> []
      in
      {
        calendar with
        yearly =
          Map.insert (date.day, date.month) (event :: events) calendar.yearly;
      }

let add_event date title description repeats calendar =
  let event = make_event title description in
  add_existing_event date event repeats calendar

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
          ~description:"Celebration of the new year" );
      (* ( Date.nth_weekday_of_month January Monday 3 year, Event.create ~id:2
         ~title:"Martin Luther King Jr. Day" ~description:"Celebration of Martin
         Luther King Jr." ); *)
      ( Date.create year February 14,
        Event.create ~id:3 ~title:"Valentine's Day"
          ~description:"Day of love and affection" );
      (* ( Date.nth_weekday_of_month February Monday 3 year, Event.create ~id:4
         ~title:"Presidents' Day" ~description:"Celebration of US Presidents"
         ); *)
      ( Date.create year March 17,
        Event.create ~id:5 ~title:"St. Patrick's Day"
          ~description:"Celebration of Irish culture" );
      (* ( easter year, Event.create ~id:6 ~title:"Easter Sunday"
         ~description:"Christian holiday celebrating the resurrection of Jesus"
         ); *)
      ( Date.create year April 22,
        Event.create ~id:7 ~title:"Earth Day"
          ~description:"Promotion of environmental awareness" );
      (* ( Date.nth_weekday_of_month May Sunday 2 year, Event.create ~id:8
         ~title:"Mother's Day" ~description:"Celebration of mothers" ); *)
      (* ( Date.last_weekday_of_month May Monday year, Event.create ~id:9
         ~title:"Memorial Day" ~description:"Honoring military personnel who
         have died" ); *)
      (* ( Date.nth_weekday_of_month June Sunday 3 year, Event.create ~id:10
         ~title:"Father's Day" ~description:"Celebration of fathers" ); *)
      ( Date.create year July 4,
        Event.create ~id:11 ~title:"Independence Day"
          ~description:"Celebration of American independence" );
      (* ( Date.nth_weekday_of_month September Monday 1 year, Event.create
         ~id:12 ~title:"Labor Day" ~description:"Honoring workers" ); (
         Date.nth_weekday_of_month October Monday 2 year, Event.create ~id:13
         ~title:"Columbus Day" ~description:"Commemoration of Christopher
         Columbus's arrival" ); *)
      ( Date.create year October 31,
        Event.create ~id:14 ~title:"Halloween"
          ~description:"Celebration involving costumes and treats" );
      ( Date.create year November 11,
        Event.create ~id:15 ~title:"Veterans Day"
          ~description:"Honoring military veterans" );
      (* ( Date.nth_weekday_of_month November Thursday 4 year, Event.create
         ~id:16 ~title:"Thanksgiving Day" ~description:"Day for giving thanks
         and family gatherings" ); *)
      ( Date.create year December 24,
        Event.create ~id:17 ~title:"Christmas Eve"
          ~description:"Day before Christmas" );
      ( Date.create year December 25,
        Event.create ~id:18 ~title:"Christmas Day"
          ~description:"Celebration of the birth of Jesus Christ" );
      ( Date.create year December 31,
        Event.create ~id:19 ~title:"New Year's Eve"
          ~description:"Celebration of the last day of the year" );
    ]
  in
  List.fold_left
    (fun cal (date, event) -> add_existing_event date event Yearly cal)
    empty fixed_dates

(** Removes event from one_time events (if it exists). *)
let remove_once_event date event calendar =
  let events = try Map.lookup date calendar.one_time with Not_found -> [] in
  let filtered_events =
    List.filter (fun e -> Event.equals e event |> not) events
  in
  { calendar with one_time = Map.insert date filtered_events calendar.one_time }

(** Removes event from daily events (if it exists). *)
let remove_daily_event event calendar =
  {
    calendar with
    daily = List.filter (fun e -> Event.equals e event |> not) calendar.daily;
  }

(** Removes event from weekly events (if it exists). *)
let remove_weekly_event date event calendar =
  let weekday = Date.day_of_week date |> Date.int_of_day_of_week in
  let arr = Array.copy calendar.weekly in
  let _ =
    arr.(weekday) <-
      List.filter (fun e -> Event.equals e event |> not) arr.(weekday)
  in
  { calendar with weekly = arr }

(** Removes event from weekly events (if it exists). *)
let remove_monthly_event (date : Date.t) event calendar =
  let d = date.day - 1 in
  let arr = Array.copy calendar.monthly in
  let _ =
    arr.(d) <- List.filter (fun e -> Event.equals e event |> not) arr.(d)
  in
  { calendar with monthly = arr }

(** Removes event from yearly events (if it exists). *)
let remove_yearly_event date event calendar =
  let d = get_day date in
  let events = try Map.lookup d calendar.yearly with Not_found -> [] in
  let filtered_events =
    List.filter (fun e -> Event.equals e event |> not) events
  in
  { calendar with yearly = Map.insert d filtered_events calendar.yearly }

let remove_event date event calendar =
  remove_once_event date event calendar
  |> remove_daily_event event
  |> remove_weekly_event date event
  |> remove_monthly_event date event
  |> remove_yearly_event date event

(** Changes event into updated event in one_time events (if it exists). *)
let edit_once_event date event updated_event calendar =
  let events = try Map.lookup date calendar.one_time with Not_found -> [] in
  let updated_events =
    List.map (fun e -> if Event.equals e event then updated_event else e) events
  in
  { calendar with one_time = Map.insert date updated_events calendar.one_time }

(** Changes event into updated event in daily events (if it exists). *)
let edit_daily_event event updated_event calendar =
  {
    calendar with
    daily =
      List.map
        (fun e -> if Event.equals e event then updated_event else e)
        calendar.daily;
  }

(** Changes event into updated event in weekly events (if it exists). *)
let edit_weekly_event date event updated_event calendar =
  let weekday = Date.day_of_week date |> Date.int_of_day_of_week in
  let arr = Array.copy calendar.weekly in
  let _ =
    arr.(weekday) <-
      List.map
        (fun e -> if Event.equals e event then updated_event else e)
        arr.(weekday)
  in
  { calendar with weekly = arr }

(** Changes event into updated event in monthly events (if it exists). *)
let edit_montly_event (date : Date.t) event updated_event calendar =
  let d = date.day - 1 in
  let arr = Array.copy calendar.monthly in
  let _ =
    arr.(d) <-
      List.map
        (fun e -> if Event.equals e event then updated_event else e)
        arr.(d)
  in
  { calendar with monthly = arr }

(** Changes event into updated event in yearly events (if it exists). *)
let edit_yearly_event date event updated_event calendar =
  let d = get_day date in
  let events = try Map.lookup d calendar.yearly with Not_found -> [] in
  let updated_events =
    List.map (fun e -> if Event.equals e event then updated_event else e) events
  in
  { calendar with yearly = Map.insert d updated_events calendar.yearly }

let edit_event date event updated_event calendar =
  edit_once_event date event updated_event calendar
  |> edit_daily_event event updated_event
  |> edit_weekly_event date event updated_event
  |> edit_montly_event date event updated_event
  |> edit_yearly_event date event updated_event

(** Finds all one-time events on given date. *)
let find_once_events date calendar =
  try Map.lookup date calendar.one_time with Not_found -> []

(** Finds all weekly events on given date. *)
let find_weekly_events date calendar =
  let weekday = Date.day_of_week date |> Date.int_of_day_of_week in
  calendar.weekly.(weekday)

(** Finds all monthly events on given date. *)
let find_monthly_events (date : Date.t) calendar =
  let d = date.day - 1 in
  calendar.monthly.(d)

(** Finds all yearly events on given date. *)
let find_yearly_events date calendar =
  let d = get_day date in
  try Map.lookup d calendar.yearly with Not_found -> []

let find_events calendar date =
  find_once_events date calendar
  @ calendar.daily
  @ find_weekly_events date calendar
  @ find_monthly_events date calendar
  @ find_yearly_events date calendar

(** Turns all one-time events into a list. *)
let list_once_events calendar =
  Map.bindings calendar.one_time
  |> List.map (fun (_, events) -> List.map Event.to_string events)
  |> List.flatten

(** Turns all daily events into a list. *)
let list_daily_events calendar = List.map Event.to_string calendar.daily

(** Turns all weekly events into a list. *)
let list_weekly_events calendar =
  Array.fold_left
    (fun acc events -> List.map Event.to_string events @ acc)
    [] calendar.weekly

(** Turns all montly events into a list. *)
let list_monthly_events calendar =
  Array.fold_left
    (fun acc events -> List.map Event.to_string events @ acc)
    [] calendar.monthly

(** Turns all yearly events into a list. *)
let list_yearly_events calendar =
  Map.bindings calendar.yearly
  |> List.map (fun (_, events) -> List.map Event.to_string events)
  |> List.flatten

let list_all_events calendar =
  list_once_events calendar @ list_daily_events calendar
  @ list_weekly_events calendar
  @ list_monthly_events calendar
  @ list_yearly_events calendar
