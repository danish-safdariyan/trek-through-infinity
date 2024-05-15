(* calendar test cases *)
open Backend
open Calendar
open OUnit2

let contains_substring str substring =
  try
    let len = String.length substring in
    for i = 0 to String.length str - len do
      if String.sub str i len = substring then raise Exit
    done;
    false
  with Exit -> true

let date = Date.create 2024 April 30

let test_add_event _ =
  let event = make_event "Meeting" "Team meeting" NoRepeat Violet in
  let calendar = add_existing_event date event empty in
  let events_on_date = find_events calendar date in
  assert_equal 1 (List.length events_on_date);
  match events_on_date with
  | [ event_in_calendar ] ->
      assert_equal (Event.get_title event) (Event.get_title event_in_calendar);
      assert_equal
        (Event.get_description event)
        (Event.get_description event_in_calendar)
  | _ -> assert_failure "Expected exactly one event on the specified date."

let test_remove_event _ =
  let event = make_event "Meeting" "Team meeting" NoRepeat Violet in
  let calendar =
    add_existing_event date event empty |> remove_event date event
  in
  assert_equal [] (find_events calendar date)

let test_edit_event _ =
  let event = make_event "Meeting" "Team meeting" NoRepeat Violet in
  let updated_event =
    Event.edit event ~title:"Updated Meeting"
      ~description:"Updated Team meeting" ~color:Yellow
  in
  let calendar =
    add_existing_event date event empty |> edit_event date event updated_event
  in
  let events_on_date = find_events calendar date in
  assert_equal 1 (List.length events_on_date);
  match events_on_date with
  | [ updated_event_from_calendar ] ->
      assert_equal "Updated Meeting"
        (Event.get_title updated_event_from_calendar);
      assert_equal "Updated Team meeting"
        (Event.get_description updated_event_from_calendar)
  | _ -> assert_failure "Expected exactly one event on the specified date."

let test_find_events _ =
  let event1 = make_event "Meeting" "Team meeting" NoRepeat Yellow in
  let event2 =
    make_event "Presentation" "Project presentation" NoRepeat Green
  in
  let calendar =
    add_existing_event date event1 empty |> add_existing_event date event2
  in
  let events_on_date = find_events calendar date in
  assert_equal 2 (List.length events_on_date);
  let titles = List.map Event.get_title events_on_date in
  let descriptions = List.map Event.get_description events_on_date in
  let expected_titles = List.sort compare [ "Meeting"; "Presentation" ] in
  let expected_descriptions =
    List.sort compare [ "Team meeting"; "Project presentation" ]
  in
  assert_equal expected_titles (List.sort compare titles);
  assert_equal expected_descriptions (List.sort compare descriptions)

let test_list_all_events _ =
  let event1 = make_event "Meeting" "Team meeting" NoRepeat Red in
  let event2 = make_event "Presentation" "Project presentation" NoRepeat Blue in
  let calendar =
    add_existing_event date event1 empty |> add_existing_event date event2
  in
  assert_equal
    [ Event.to_string event2; Event.to_string event1 ]
    (list_all_events calendar)

let test_add_yearly_events _ =
  let event1 = make_event "New Year's Day" "Celebration" Yearly Green in
  let event2 =
    make_event "Valentine's Day" "Valentine's celebration" Yearly Red
  in
  let calendar =
    add_existing_event (Date.create 2024 January 1) event1 empty
    |> add_existing_event (Date.create 2024 February 14) event2
  in
  let events_jan_1 = find_events calendar (Date.create 2024 January 1) in
  let events_feb_14 = find_events calendar (Date.create 2024 February 14) in
  assert_equal 1 (List.length events_jan_1);
  assert_equal 1 (List.length events_feb_14);
  assert_equal "New Year's Day" (Event.get_title (List.hd events_jan_1));
  assert_equal "Celebration" (Event.get_description (List.hd events_jan_1));
  assert_equal "Valentine's Day" (Event.get_title (List.hd events_feb_14));
  assert_equal "Valentine's celebration"
    (Event.get_description (List.hd events_feb_14))

let test_easter _ =
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
      assert_equal expected_date calculated_date)
    known_easter_dates

let test_initialize_calendar _ =
  let calendar = initialize_calendar empty in
  let event_descriptions = list_all_events calendar in
  assert_bool "New Year's Day event is missing"
    (List.exists
       (fun desc -> contains_substring desc "Title: New Year's Day")
       event_descriptions);
  assert_bool "Easter Sunday event is missing"
    (List.exists
       (fun desc -> contains_substring desc "Title: Easter Sunday")
       event_descriptions)

let add_days date days =
  let rec add d count =
    if count = 0 then d
    else if count > 0 then add (Date.next_day d) (count - 1)
    else add (Date.prev_day d) (count + 1)
  in
  add date days

let add_weeks date weeks = add_days date (weeks * 7)

let add_months date months =
  let rec add d count =
    if count = 0 then d
    else if count > 0 then
      let month, year = Date.next_month (Date.get_month d) (Date.get_year d) in
      add (Date.create year month d.day) (count - 1)
    else
      let month, year = Date.prev_month (Date.get_month d) (Date.get_year d) in
      add (Date.create year month d.day) (count + 1)
  in
  add date months

let add_years (date : Date.t) years =
  Date.create (date.year + years) date.month date.day

let test_make_event_no_repeat _ =
  let calendar = initialize_calendar empty in
  let new_calendar =
    add_event date "Meeting" "Team meeting" NoRepeat Red calendar
  in
  let events = Calendar.find_events new_calendar date in
  assert_equal 1 (List.length events)

let test_make_event_daily_repeat _ =
  let calendar = initialize_calendar empty in
  let new_calendar =
    add_event date "Daily Standup" "Daily team sync" Daily Violet calendar
  in
  for i = 0 to 5 do
    let test_forward = add_days date i in
    let test_backward = add_days date (-i - 1) in
    let events = Calendar.find_events new_calendar test_forward in
    assert_equal 1 (List.length events);
    let events = Calendar.find_events new_calendar test_backward in
    assert_equal 0 (List.length events)
  done

let test_make_event_weekly_repeat _ =
  let calendar = initialize_calendar empty in
  let updated_calendar =
    Calendar.add_event date "Weekly Meeting" "Weekly team meeting" Weekly Yellow
      calendar
  in
  for i = 0 to 5 do
    let test_forward = add_weeks date i in
    let test_backward = add_weeks date (-i - 1) in
    let events = Calendar.find_events updated_calendar test_forward in
    assert_equal 1 (List.length events);
    let events = Calendar.find_events updated_calendar test_backward in
    assert_equal 0 (List.length events)
  done

let test_make_event_monthly_repeat _ =
  let calendar = initialize_calendar empty in
  let updated_calendar =
    add_event date "Monthly Review" "Monthly performance review" Monthly Blue
      calendar
  in
  for i = 0 to 5 do
    let test_forward = add_months date i in
    let test_backward = add_months date (-i - 1) in
    let events = Calendar.find_events updated_calendar test_forward in
    assert_equal 1 (List.length events);
    let events = Calendar.find_events updated_calendar test_backward in
    assert_equal 0 (List.length events)
  done

let test_make_event_yearly_repeat _ =
  let calendar = initialize_calendar empty in
  let updated_calendar =
    Calendar.add_event date "Annual Conference" "Yearly strategy conference"
      Yearly Green calendar
  in
  for i = 0 to 5 do
    let test_forward = add_years date i in
    let test_backward = add_years date (-i - 1) in
    let events = Calendar.find_events updated_calendar test_forward in
    assert_equal 1 (List.length events);
    let events = Calendar.find_events updated_calendar test_backward in
    assert_equal 0 (List.length events)
  done

(* Test adding and finding events in a leap year *)
let test_leap_year_event _ =
  let date = Date.create 2024 February 29 in
  let event =
    make_event "Leap Day Meeting" "Meeting on leap day" NoRepeat Blue
  in
  let calendar = add_existing_event date event empty in
  let events = find_events calendar date in
  assert_equal 1 (List.length events)

(* Test adding multiple events on the same day and ensure all are found *)
let test_multiple_events_same_day _ =
  let date = Date.create 2024 March 1 in
  let event1 =
    make_event "Morning Meeting" "Team meeting in the morning" NoRepeat Red
  in
  let event2 =
    make_event "Evening Gathering" "Casual team gathering in the evening"
      NoRepeat Green
  in
  let calendar =
    add_existing_event date event1 empty |> add_existing_event date event2
  in
  let events = find_events calendar date in
  assert_equal 2 (List.length events)

(* Helper function to add days to a date *)
let add_days date days =
  let rec aux d count =
    if count > 0 then aux (Date.next_day d) (count - 1)
    else if count < 0 then aux (Date.prev_day d) (count + 1)
    else d
  in
  aux date days

(* Test removal of a weekly recurring event *)
let test_weekly_event_continuation _ =
  let start_date = Date.create 2024 March 3 in
  let event =
    make_event "Weekly Meeting" "Discussion and updates" Weekly Green
  in
  let calendar = add_existing_event start_date event empty in
  let next_week_date = add_days start_date 7 in
  let two_weeks_date = add_days start_date 14 in
  assert_equal true
    (List.exists
       (fun e -> Event.get_title e = "Weekly Meeting")
       (find_events calendar next_week_date));
  assert_equal true
    (List.exists
       (fun e -> Event.get_title e = "Weekly Meeting")
       (find_events calendar two_weeks_date))

(* Test editing a monthly recurring event *)
let test_edit_monthly_event _ =
  let date = Date.create 2024 April 1 in
  let event =
    make_event "Monthly Review" "Monthly project review" Monthly Blue
  in
  let updated_event =
    Event.edit event ~title:"Updated Monthly Review"
      ~description:"Updated monthly project review" ~color:Orange
  in
  let calendar =
    add_existing_event date event empty |> edit_event date event updated_event
  in
  let next_month_date = Date.create 2024 May 1 in
  let events = find_events calendar next_month_date in
  assert_equal 1 (List.length events);
  assert_equal "Updated Monthly Review" (Event.get_title (List.hd events))

(* Test initialization of calendar with edge cases like December 31st *)
let test_initialize_calendar_edge_cases _ =
  let year = 2024 in
  let new_years_eve = Date.create year December 31 in
  let new_years_day = Date.create year January 1 in
  let calendar = initialize_calendar empty in
  let events_dec_31 = find_events calendar new_years_eve in
  let events_jan_1 = find_events calendar new_years_day in
  assert_bool "Missing New Year's Eve event"
    (List.exists (fun e -> Event.get_title e = "New Year's Eve") events_dec_31);
  assert_bool "Missing New Year's Day event"
    (List.exists (fun e -> Event.get_title e = "New Year's Day") events_jan_1)

let suite =
  "Calendar Tests"
  >::: [
         "test_add_event" >:: test_add_event;
         "test_remove_event" >:: test_remove_event;
         "test_edit_event" >:: test_edit_event;
         "test_find_events" >:: test_find_events;
         "test_list_all_events" >:: test_list_all_events;
         "test_add_yearly_events" >:: test_add_yearly_events;
         "test_easter" >:: test_easter;
         "test_initialize_calendar" >:: test_initialize_calendar;
         "test_make_event_no_repeat" >:: test_make_event_no_repeat;
         "test_make_event_daily_repeat" >:: test_make_event_daily_repeat;
         "test_make_event_weekly_repeat" >:: test_make_event_weekly_repeat;
         "test_make_event_monthly_repeat" >:: test_make_event_monthly_repeat;
         "test_make_event_yearly_repeat" >:: test_make_event_yearly_repeat;
         "test_leap_year_event" >:: test_leap_year_event;
         "test_multiple_events_same_day" >:: test_multiple_events_same_day;
         "test_weekly_event_continuation" >:: test_weekly_event_continuation;
         "test_edit_monthly_event" >:: test_edit_monthly_event;
         "test_initialize_calendar_edge_cases"
         >:: test_initialize_calendar_edge_cases;
       ]

let () =
  run_test_tt_main suite;
  print_endline "test_calendar passed all tests"
