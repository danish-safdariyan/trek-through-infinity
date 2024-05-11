open Backend
open Date
open Calendar
open Event

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
  let calendar = empty in
  let event =
    Event.create ~id:1 ~title:"Meeting" ~description:"Team meeting"
      ~date:"2024-04-30" ~repeats:NoRepeat
  in
  let calendar_with_event = add_event calendar date event in
  (* Check if the event is added to the calendar *)
  assert (find_events calendar_with_event date = [ event ])

let test_remove_event () =
  let event =
    Event.create ~id:1 ~title:"Meeting" ~description:"Team meeting"
      ~date:"2024-04-30" ~repeats:NoRepeat
  in
  let calendar = add_event empty date event in
  let calendar_without_event = remove_event calendar date 1 in
  (* Check if the event is removed from the calendar *)
  assert (find_events calendar_without_event date = [])

let test_edit_event () =
  let event =
    Event.create ~id:1 ~title:"Meeting" ~description:"Team meeting"
      ~date:"2024-04-30" ~repeats:NoRepeat
  in
  let updated_event =
    Event.edit event ~title:"Updated Meeting"
      ~description:"Updated Team meeting" ~date:"2024-05-01" ~repeats:Weekly
  in
  let calendar = add_event empty date event in
  let calendar_with_updated_event = edit_event calendar date 1 updated_event in
  (* Check if the event is updated in the calendar *)
  assert (find_events calendar_with_updated_event date = [ updated_event ])

let test_find_events () =
  (* Adjust this as per your date handling *)
  let event1 =
    Event.create ~id:1 ~title:"Meeting" ~description:"Team meeting"
      ~date:"2024-04-30" ~repeats:NoRepeat
  in
  let event2 =
    Event.create ~id:2 ~title:"Presentation"
      ~description:"Project\n presentation" ~date:"2024-04-30" ~repeats:NoRepeat
  in
  let calendar = add_event empty date event1 in
  let calendar_with_multiple_events = add_event calendar date event2 in
  let events_on_date = find_events calendar_with_multiple_events date in
  Printf.printf "Events found: %d\n" (List.length events_on_date);
  List.iter
    (fun e -> Printf.printf "Event: %s\n" (Event.to_string e))
    events_on_date;
  assert (events_on_date = [ event2; event1 ])

let test_list_all_events () =
  let event1 =
    Event.create ~id:1 ~title:"Meeting" ~description:"Team meeting"
      ~date:"2024-04-30" ~repeats:NoRepeat
  in
  let event2 =
    Event.create ~id:2 ~title:"Presentation" ~description:"Project presentation"
      ~date:"2024-05-01" ~repeats:NoRepeat
  in
  let calendar = add_event empty date event1 in
  let calendar_with_event_on_next_day = add_event calendar date event2 in
  (* Check if all events are listed *)
  assert (
    list_all_events calendar_with_event_on_next_day
    = [ Event.to_string event2; Event.to_string event1 ])

let test_add_spe_events () =
  let events =
    [
      ( Date.create 2024 January 1,
        Event.create ~id:1 ~title:"New Year's Day" ~description:"Celebration"
          ~date:"2024-01-01" ~repeats:Yearly );
      ( Date.create 2024 February 14,
        Event.create ~id:2 ~title:"Valentine's Day"
          ~description:"Valentine's\n   celebration" ~date:"2024-02-14"
          ~repeats:Yearly );
    ]
  in
  let calendar = add_events empty events in
  assert (
    find_events calendar (Date.create 2024 January 1)
    = [ List.assoc (Date.create 2024 January 1) events ]);
  assert (
    find_events calendar (Date.create 2024 February 14)
    = [ List.assoc (Date.create 2024 February 14) events ])

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
  test_add_spe_events ();
  test_easter ();
  test_initialize_calendar ();
  print_endline "All test cases passed!"
