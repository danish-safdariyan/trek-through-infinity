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
  print_endline "All test cases passed!"
