open OUnit
open Trek
open Event
open Map
include Event_holder
include EventHolder

let test_empty _ =
  let events = EventHolder.empty in
  assert_equal 0 (Map.cardinal events) ~msg:"Empty map should have 0 events"

let test_insert_find_remove _ =
  let events = EventHolder.empty in
  let event1 =
    EventHolder.Event.create ~id:1 ~title:"Event 1" ~description:"Description 1"
      ~date:"2024-05-01" ~repeats:EventHolder.Event.NoRepeat
  in
  let event2 =
    EventHolder.Event.create ~id:2 ~title:"Event 2" ~description:"Description 2"
      ~date:"2024-05-01" ~repeats:EventHolder.Event.NoRepeat
  in
  let event3 =
    EventHolder.Event.create ~id:3 ~title:"Event 3" ~description:"Description 3"
      ~date:"2024-05-02" ~repeats:EventHolder.Event.NoRepeat
  in

  let events = EventHolder.insert "2024-05-01" event1 events in
  let events = EventHolder.insert "2024-05-01" event2 events in
  let events = EventHolder.insert "2024-05-02" event3 events in

  match EventHolder.find "2024-05-01" events with
  | Some events ->
      assert_equal 2 (Array.length events)
        ~msg:"Expected 2 events for 2024-05-01"
  | None -> assert_failure "Events for 2024-05-01 not found"

let test_remove _ =
  let events = EventHolder.empty in
  let event1 =
    EventHolder.Event.create ~id:1 ~title:"Event 1" ~description:"Description 1"
      ~date:"2024-05-01" ~repeats:EventHolder.Event.NoRepeat
  in
  let event2 =
    EventHolder.Event.create ~id:2 ~title:"Event 2" ~description:"Description 2"
      ~date:"2024-05-01" ~repeats:EventHolder.Event.NoRepeat
  in

  let events = EventHolder.insert "2024-05-01" event1 events in
  let events = EventHolder.insert "2024-05-01" event2 events in

  let events = EventHolder.remove "2024-05-01" events in

  assert_equal None
    (EventHolder.find "2024-05-01" events)
    ~msg:"Events for 2024-05-01 should have been removed"

let suite =
  "EventHolder tests"
  >::: [
         "test_empty" >:: test_empty;
         "test_insert_find_remove" >:: test_insert_find_remove;
         "test_remove" >:: test_remove;
       ]

let test = run_test_tt_main suite
