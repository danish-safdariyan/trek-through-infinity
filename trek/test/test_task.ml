open OUnit2
open Backend
open Task

let test_create _ =
  let task =
    Task.create ~title:"Task 1" ~date:"2024-05-13" ~display:CalDisplay
  in
  assert_equal "Task 1" (Task.get_title task);
  assert_equal "2024-05-13" (Task.get_date task);
  assert_equal CalDisplay (Task.get_display task)

let test_edit_task _ =
  let task =
    Task.create ~title:"Task 1" ~date:"2024-05-13" ~display:CalDisplay
  in
  let edited_task =
    Task.edit_task task ~title:"Updated Task" ~date:"2024-05-14"
      ~display:ListDisplay
  in
  assert_equal "Updated Task" (Task.get_title edited_task);
  assert_equal "2024-05-14" (Task.get_date edited_task);
  assert_equal ListDisplay (Task.get_display edited_task)

let test_to_string _ =
  let task =
    Task.create ~title:"Task 1" ~date:"2024-05-13" ~display:BothDisplay
  in
  let expected_string =
    "Title: Task 1, Date: 2024-05-13, Display:\n   Display on Calendar and List"
  in
  assert_equal expected_string (Task.to_string task)

let suite =
  "Task Tests"
  >::: [
         "test_create" >:: test_create;
         "test_edit_task" >:: test_edit_task;
         "test_to_string" >:: test_to_string;
       ]

let () =
  run_test_tt_main suite;
  print_endline "test_task passed all tests"
