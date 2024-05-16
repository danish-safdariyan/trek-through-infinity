open OUnit2
open Backend
open Task

let date1 = Date.create 2024 May 13
let date2 = Date.create 2024 May 14

let test_create _ =
  let task = create ~title:"Task 1" ~date:date1 in
  assert_equal "Task 1" (get_title task);
  assert_equal date1 (get_date task)

let test_edit_update_title _ =
  let task = create ~title:"Task 1" ~date:date1 in
  let edited_task = edit_task task ~title:"Updated Task" ~date:date1 in
  assert_equal "Updated Task" (get_title edited_task);
  assert_equal date1 (get_date edited_task)

let test_edit_update_date _ =
  let task = create ~title:"Task 1" ~date:date1 in
  let edited_task = edit_task task ~title:"Task 1" ~date:date2 in
  assert_equal "Task 1" (get_title edited_task);
  assert_equal date2 (get_date edited_task)

let test_edit_update_all _ =
  let task = create ~title:"Task 1" ~date:date1 in
  let edited_task = edit_task task ~title:"Updated Task" ~date:date2 in
  assert_equal "Updated Task" (get_title edited_task);
  assert_equal date2 (get_date edited_task)

let test_to_string _ =
  let task = create ~title:"Task 1" ~date:date1 in
  let expected_string = "Title: Task 1, Date: 2024-05-13" in
  assert_equal expected_string (to_string task)

let test_compare_equal _ =
  let task1 = create ~title:"Task 1" ~date:date1 in
  let task2 = create ~title:"Task 1" ~date:date1 in
  assert_equal true (equals task1 task2)

let test_compare_diff_title _ =
  let task1 = create ~title:"Task 1" ~date:date1 in
  let task2 = create ~title:"Task 2" ~date:date1 in
  assert_equal false (equals task1 task2)

let test_compare_diff_date _ =
  let task1 = create ~title:"Task 1" ~date:date1 in
  let task2 = create ~title:"Task 1" ~date:date2 in
  assert_equal false (equals task1 task2)

let suite =
  "Task Tests"
  >::: [
         "test_create" >:: test_create;
         "test_edit_update_title" >:: test_edit_update_title;
         "test_edit_update_date" >:: test_edit_update_date;
         "test_edit_update_all" >:: test_edit_update_all;
         "test_to_string_calendar_display" >:: test_to_string;
         "test_compare_equal" >:: test_compare_equal;
         "test_compare_diff_title" >:: test_compare_diff_title;
         "test_compare_diff_date" >:: test_compare_diff_date;
       ]

let () =
  run_test_tt_main suite;
  print_endline "test_task passed all tests"
