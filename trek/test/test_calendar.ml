open Backend
open Date
open Calendar

(* Import the Calendar module *)
let contains_substring str substring =
  try
    let len = String.length substring in
    for i = 0 to String.length str - len do
      if String.sub str i len = substring then raise Exit
    done;
    false
  with Exit -> true

let date = Date.create 2024 April 30

let test_add_event () =
  let event = make_event "Meeting" "Team meeting" in
  let calendar = add_existing_event date event NoRepeat empty in
  (* Check if the event is added to the calendar *)
  assert (find_events calendar date = [ event ])

let test_remove_event () =
  let event = make_event "Meeting" "Team meeting" in
  let calendar =
    add_existing_event date event NoRepeat empty |> remove_event date event
  in
  (* Check if the event is removed from the calendar *)
  assert (find_events calendar date = [])

let test_edit_event () =
  let event = make_event "Meeting" "Team meeting" in
  let updated_event =
    Event.edit event ~title:"Updated Meeting"
      ~description:"Updated Team meeting"
  in
  let calendar =
    add_existing_event date event NoRepeat empty
    |> edit_event date event updated_event
  in
  (* Check if the event is updated in the calendar *)
  assert (find_events calendar date = [ updated_event ])

let test_find_events () =
  (* Adjust this as per your date handling *)
  let event1 = make_event "Meeting" "Team meeting" in
  let event2 = make_event "Presentation" "Project\n presentation" in
  let calendar =
    add_existing_event date event1 NoRepeat empty
    |> add_existing_event date event2 NoRepeat
  in
  let events_on_date = find_events calendar date in
  Printf.printf "Events found: %d\n" (List.length events_on_date);
  List.iter
    (fun e -> Printf.printf "Event: %s\n" (Event.to_string e))
    events_on_date;
  assert (events_on_date = [ event2; event1 ])

let test_list_all_events () =
  let event1 = make_event "Meeting" "Team meeting" in
  let event2 = make_event "Presentation" "Project presentation" in
  let calendar =
    add_existing_event date event1 NoRepeat empty
    |> add_existing_event date event2 NoRepeat
  in
  (* Check if all events are listed *)
  assert (
    list_all_events calendar
    = [ Event.to_string event2; Event.to_string event1 ])

let test_add_yearly_events () =
  let event1 = make_event "New Year's Day" "Celebration" in
  let event2 = make_event "Valentine's Day" "Valentine's\n   celebration" in
  let calendar =
    add_existing_event (Date.create 1 January 2024) event1 Yearly empty
    |> add_existing_event (Date.create 14 February 2024) event2 Yearly
  in
  assert (find_events calendar (Date.create 2024 January 1) = [ event1 ]);
  assert (find_events calendar (Date.create 2024 February 14) = [ event2 ])

let test_easter () =
  let known_easter_dates =
    [
      (2024, Date.create 2024 March 31);
      (2025, Date.create 2025 April 20);
      (2026, Date.create 2026 April 5);
    ]
  in
  List.iter
    (fun (year, expected_date) ->
      let calculated_date = easter year in
      Printf.printf "Testing year %d: Expected %s, got %s\n" year
        (Date.to_string expected_date)
        (Date.to_string calculated_date);
      assert (calculated_date = expected_date))
    known_easter_dates

let test_initialize_calendar () =
  let year = 2024 in
  let calendar = initialize_calendar year in
  let event_descriptions = list_all_events calendar in

  (* Debugging output to inspect what's being tested *)
  List.iter
    (fun desc -> Printf.printf "Event Description: %s\n" desc)
    event_descriptions;

  (* Adjusted assertions to check for substring presence within the full event
     description *)
  assert (
    List.exists
      (fun desc -> contains_substring desc "Title: New Year's Day")
      event_descriptions);
  assert (
    List.exists
      (fun desc -> contains_substring desc "Title: Easter Sunday")
      event_descriptions)

(* Helper function to convert a string to a date using the Date module's
   parse_date function *)
let string_to_date str = Date.parse_date str

(* Helper function to add days to a date, since the original add_days function
   is not directly available *)
let add_days date days =
  let rec add d count =
    if count = 0 then d else add (Date.next_day d) (count - 1)
  in
  add date days

(* Helper function to add weeks, simply reuses add_days *)
let add_weeks date weeks = add_days date (weeks * 7)

(* Helper function to add months *)
let add_months date months =
  let rec add d count =
    if count = 0 then d
    else
      let month, year = Date.next_month (Date.get_month d) (Date.get_year d) in
      add { year; month; day = Date.get_day d } (count - 1)
  in
  add date months

(* Helper function to add years *)
let add_years date years = { date with year = date.year + years }

(* Test cases for make_event *)
let test_make_event_no_repeat () =
  let calendar = Calendar.empty in
  let date = string_to_date "2024-04-30" in
  let updated_calendar =
    Calendar.make_event ~title:"Meeting" ~description:"Team meeting"
      ~date:"2024-04-30" ~repeats:Event.NoRepeat ~times:1 ~calendar
  in
  let events = Calendar.find_events updated_calendar date in
  assert (List.length events = 1);
  Printf.printf "Test make_event_no_repeat passed!\n"

let test_make_event_daily_repeat () =
  let calendar = Calendar.empty in
  let date = string_to_date "2024-04-30" in
  let repeats = 5 in
  let updated_calendar =
    Calendar.make_event ~title:"Daily Standup" ~description:"Daily team sync"
      ~date:"2024-04-30" ~repeats:Event.Daily ~times:repeats ~calendar
  in
  for i = 0 to repeats - 1 do
    let event_date = add_days date i in
    let events = Calendar.find_events updated_calendar event_date in
    assert (List.length events = 1)
  done;
  Printf.printf "Test make_event_daily_repeat passed!\n"

(* Test case for weekly event repeats *)
let test_make_event_weekly_repeat () =
  let calendar = Calendar.empty in
  let date = string_to_date "2024-04-30" in
  let repeats = 4 in
  let updated_calendar =
    Calendar.make_event ~title:"Weekly Meeting"
      ~description:"Weekly team meeting" ~date:"2024-04-30"
      ~repeats:Event.Weekly ~times:repeats ~calendar
  in
  for i = 0 to repeats - 1 do
    let event_date = add_weeks date i in
    let events = Calendar.find_events updated_calendar event_date in
    assert (List.length events = 1)
  done;
  Printf.printf "Test make_event_weekly_repeat passed!\n"

(* Test case for monthly event repeats *)
let test_make_event_monthly_repeat () =
  let calendar = Calendar.empty in
  let date = string_to_date "2024-01-01" in
  let repeats = 3 in
  let updated_calendar =
    Calendar.make_event ~title:"Monthly Review"
      ~description:"Monthly performance review" ~date:"2024-01-01"
      ~repeats:Event.Monthly ~times:repeats ~calendar
  in
  for i = 0 to repeats - 1 do
    let event_date = add_months date i in
    let events = Calendar.find_events updated_calendar event_date in
    assert (List.length events = 1)
  done;
  Printf.printf "Test make_event_monthly_repeat passed!\n"

(* Test case for yearly event repeats *)
let test_make_event_yearly_repeat () =
  let calendar = Calendar.empty in
  let date = string_to_date "2024-01-01" in
  let repeats = 2 in
  let updated_calendar =
    Calendar.make_event ~title:"Annual Conference"
      ~description:"Yearly strategy conference" ~date:"2024-01-01"
      ~repeats:Event.Yearly ~times:repeats ~calendar
  in
  for i = 0 to repeats - 1 do
    let event_date = add_years date i in
    let events = Calendar.find_events updated_calendar event_date in
    assert (List.length events = 1)
  done;
  Printf.printf "Test make_event_yearly_repeat passed!\n"

(* Run the test cases *)
let () =
  test_add_event ();
  test_remove_event ();
  test_edit_event ();
  test_find_events ();
  test_list_all_events ();
  test_add_yearly_events ();
  test_easter ();
  test_initialize_calendar ();
  test_make_event_no_repeat ();
  test_make_event_daily_repeat ();
  test_make_event_weekly_repeat ();
  test_make_event_monthly_repeat ();
  test_make_event_yearly_repeat ();
  print_endline "All test cases passed!"
