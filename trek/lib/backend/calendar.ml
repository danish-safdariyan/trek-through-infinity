(* calendar.ml *)
(** Implementation of the Calendar module for managing events *)

module Map = CalDict.AssocListMap

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
let make_event title description repeats color =
  let id = next_id () in
  Event.create ~id ~title ~description ~repeats ~color

(* Add an existing event to the calendar. *)
let add_existing_event date event calendar =
  match Event.get_repeats event with
  | NoRepeat ->
      let events =
        try Map.lookup date calendar.one_time with Not_found -> []
      in
      {
        calendar with
        one_time = Map.insert date (event :: events) calendar.one_time;
      }
  | Daily ->
      let e = Event.add_condition (fun d -> Date.compare d date >= 0) event in
      { calendar with daily = e :: calendar.daily }
  | Weekly ->
      let e = Event.add_condition (fun d -> Date.compare d date >= 0) event in
      let weekday = Date.day_of_week date |> Date.int_of_day_of_week in
      let arr = Array.copy calendar.weekly in
      let _ = arr.(weekday) <- e :: Array.get arr weekday in
      { calendar with weekly = arr }
  | Monthly ->
      let e = Event.add_condition (fun d -> Date.compare d date >= 0) event in
      let d = date.day - 1 in
      let arr = Array.copy calendar.monthly in
      let _ = arr.(d) <- e :: Array.get arr d in
      { calendar with monthly = arr }
  | Yearly ->
      let e = Event.add_condition (fun d -> Date.compare d date >= 0) event in
      let events =
        try Map.lookup (date.day, date.month) calendar.yearly
        with Not_found -> []
      in
      {
        calendar with
        yearly = Map.insert (date.day, date.month) (e :: events) calendar.yearly;
      }

(* Create and add an event to the calendar. *)
let add_event date title description repeats color calendar =
  let event = make_event title description repeats color in
  add_existing_event date event calendar

(* Calculate the date of Easter for a given year. *)
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

(* Add condition to event for nth weekday of the month. *)
let make_nth_weekday_of_month_event month weekday n =
  Event.add_condition (fun d ->
      Date.compare d (Date.nth_weekday_of_month month weekday n d.year) = 0)

(* Recursively add recurring events to the calendar. *)
let rec add_forever_events calendar = function
  | [] -> calendar
  | (date, event) :: t ->
      add_forever_events
        (match Event.get_repeats event with
        | Weekly ->
            let weekday = Date.day_of_week date |> Date.int_of_day_of_week in
            let arr = Array.copy calendar.weekly in
            let _ = arr.(weekday) <- event :: Array.get arr weekday in
            { calendar with weekly = arr }
        | Yearly ->
            let e =
              Event.add_condition (fun d -> Date.compare d date >= 0) event
            in
            let events =
              try Map.lookup (date.day, date.month) calendar.yearly
              with Not_found -> []
            in
            {
              calendar with
              yearly =
                Map.insert (date.day, date.month) (e :: events) calendar.yearly;
            }
        | _ -> calendar)
        t

(* Initialize calendar with annual events *)
let initialize_calendar cal =
  let year = (Date.current_date ()).year in
  let fixed_dates =
    [
      ( Date.create year January 1,
        make_event "New Year's Day" "Celebration of the new year" Yearly Green
      );
      ( Date.nth_weekday_of_month January Monday 3 year,
        make_event "Martin Luther King Jr. Day"
          "Celebration of Martin\n         Luther King Jr." Yearly Yellow
        |> make_nth_weekday_of_month_event January Monday 3 );
      ( Date.create year February 14,
        make_event "Valentine's Day" "Day of love and affection" Yearly Red );
      ( Date.nth_weekday_of_month February Monday 3 year,
        make_event "Presidents' Day" "Celebration of US Presidents" Yearly Blue
        |> make_nth_weekday_of_month_event February Monday 3 );
      ( Date.create year March 17,
        make_event "St. Patrick's Day" "Celebration of Irish culture" Yearly
          Violet );
      ( easter year,
        make_event "Easter Sunday"
          "Christian holiday celebrating the resurrection of Jesus" Weekly
          Orange
        |> Event.add_condition (fun d -> Date.compare d (easter d.year) = 0) );
      ( Date.create year April 22,
        make_event "Earth Day" "Promotion of environmental awareness" Yearly
          Green );
      ( Date.nth_weekday_of_month May Sunday 2 year,
        make_event "Mother's Day" "Celebration of mothers" Weekly Red
        |> make_nth_weekday_of_month_event May Sunday 2 );
      ( Date.last_weekday_of_month May Monday year,
        make_event "Memorial Day" "Honoring military personnel who have died"
          Weekly Yellow
        |> Event.add_condition (fun d ->
               Date.compare d (Date.last_weekday_of_month May Monday d.year) = 0)
      );
      ( Date.nth_weekday_of_month June Sunday 3 year,
        make_event "Father's Day" "Celebration of fathers" Weekly Red
        |> make_nth_weekday_of_month_event June Sunday 3 );
      ( Date.create year July 4,
        make_event "Independence Day" "Celebration of American independence"
          Yearly Violet );
      ( Date.nth_weekday_of_month September Monday 1 year,
        make_event "Labor Day" "Honoring workers" Weekly Indigo
        |> make_nth_weekday_of_month_event September Monday 1 );
      ( Date.nth_weekday_of_month October Monday 2 year,
        make_event "Columbus Day"
          "Commemoration of Christopher Columbus's arrival" Weekly Indigo
        |> make_nth_weekday_of_month_event October Monday 2 );
      ( Date.create year October 31,
        make_event "Halloween" "Celebration involving costumes and treats"
          Yearly Orange );
      ( Date.create year November 11,
        make_event "Veterans Day" "Honoring military veterans" Yearly Yellow );
      ( Date.nth_weekday_of_month November Thursday 4 year,
        make_event "Thanksgiving Day"
          "Day for giving thanks and family gatherings" Weekly Green
        |> make_nth_weekday_of_month_event November Thursday 4 );
      ( Date.create year December 24,
        make_event "Christmas Eve" "Day before Christmas" Yearly Green );
      ( Date.create year December 25,
        make_event "Christmas Day" "Celebration of the birth of Jesus Christ"
          Yearly Red );
      ( Date.create year December 31,
        make_event "New Year's Eve" "Celebration of the last day of the year"
          Yearly Green );
    ]
  in
  add_forever_events cal fixed_dates

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

(** Remove an event from the calendar. *)
let remove_event date event calendar =
  match Event.get_repeats event with
  | NoRepeat -> remove_once_event date event calendar
  | Daily -> remove_daily_event event calendar
  | Weekly -> remove_weekly_event date event calendar
  | Monthly -> remove_monthly_event date event calendar
  | Yearly -> remove_yearly_event date event calendar

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

(** Edit an event in the calendar. *)
let edit_event date event updated_event calendar =
  match Event.get_repeats event with
  | NoRepeat -> edit_once_event date event updated_event calendar
  | Daily -> edit_daily_event event updated_event calendar
  | Weekly -> edit_weekly_event date event updated_event calendar
  | Monthly -> edit_montly_event date event updated_event calendar
  | Yearly -> edit_yearly_event date event updated_event calendar

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

(** Find all events on a given date. *)
let find_events calendar date =
  find_once_events date calendar
  @ calendar.daily
  @ find_weekly_events date calendar
  @ find_monthly_events date calendar
  @ find_yearly_events date calendar
  |> List.filter (Event.event_on_day date)

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

(** List all events in the calendar. *)
let list_all_events calendar =
  list_once_events calendar @ list_daily_events calendar
  @ list_weekly_events calendar
  @ list_monthly_events calendar
  @ list_yearly_events calendar

let save_calendar calendar filename =
  let oc = open_out filename in
  Marshal.to_channel oc calendar [];
  close_out oc

(* Load the calendar from a file. *)
let load_calendar filename =
  let ic = open_in filename in
  let cal : t = Marshal.from_channel ic in
  close_in ic;
  cal
