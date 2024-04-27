open Trek
open Date
open Calendar
open Event

(* Import the Calendar module *)

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
  let event1 =
    Event.create ~id:1 ~title:"Meeting" ~description:"Team meeting"
      ~date:"2024-04-30" ~repeats:NoRepeat
  in
  let event2 =
    Event.create ~id:2 ~title:"Presentation" ~description:"Project presentation"
      ~date:"2024-04-30" ~repeats:NoRepeat
  in
  let calendar = add_event empty date event1 in
  let calendar_with_multiple_events = add_event calendar date event2 in
  (* Check if both events are found for the given date *)
  assert (find_events calendar_with_multiple_events date = [ event1; event2 ])

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
    = [ Event.to_string event1; Event.to_string event2 ])

(* Run the test cases *)
let () =
  test_add_event ();
  test_remove_event ();
  test_edit_event ();
  test_find_events ();
  test_list_all_events ();
  print_endline "All test cases passed!"
