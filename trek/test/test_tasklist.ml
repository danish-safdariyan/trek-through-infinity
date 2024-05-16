open OUnit2
open Backend
open TaskList

let date1 = Date.create 2024 May 13
let date2 = Date.create 2024 May 14

let test_empty_task_list _ =
  let empty_list = empty in
  assert_equal empty_list empty

let test_add_task_to_empty_list _ =
  let task1 = Task.create ~title:"Task 1" ~date:date1 in
  let list_with_task1 = add_task task1 empty in
  assert_equal list_with_task1 (add_task task1 empty)

let test_remove_task_one_ele _ =
  let task1 = Task.create ~title:"Task 1" ~date:date1 in
  let list_with_task1 = add_task task1 empty in
  let list_without_task1 = remove_task task1 list_with_task1 in
  assert_equal (list_tasks empty) (list_tasks list_without_task1)

let test_remove_task_two_ele _ =
  let task1 = Task.create ~title:"Task 1" ~date:date1 in
  let task2 = Task.create ~title:"Task 2" ~date:date1 in
  let list_with_both = add_task task1 empty |> add_task task2 in
  let list_without_task1 = remove_task task1 list_with_both in
  let list_with_task2 = add_task task2 empty in
  assert_equal (list_tasks list_with_task2) (list_tasks list_without_task1)

let test_remove_task_absent _ =
  let task1 = Task.create ~title:"Task 1" ~date:date1 in
  let task2 = Task.create ~title:"Task 2" ~date:date1 in
  let list_with_task1 = add_task task1 empty in
  let remove_attempt = remove_task task2 list_with_task1 in
  let correct_list = add_task task1 empty in
  assert_equal (list_tasks remove_attempt) (list_tasks correct_list)

let test_replace_task _ =
  let task1 = Task.create ~title:"Task 1" ~date:date1 in
  let list_with_task1 = add_task task1 empty in
  let task2 = Task.create ~title:"Updated Task" ~date:date2 in
  let updated_list = replace_task task1 task2 list_with_task1 in
  assert_equal (list_tasks updated_list) (add_task task2 empty |> list_tasks)

let test_list_tasks _ =
  let task1 = Task.create ~title:"Task 1" ~date:date1 in
  let task2 = Task.create ~title:"Task 2" ~date:date2 in
  let list_with_tasks = add_task task1 empty in
  let list_with_all_tasks = add_task task2 list_with_tasks in
  assert_equal
    [ "Title: Task 1, Date: 2024-05-13"; "Title: Task 2, Date: 2024-05-14" ]
    (list_tasks list_with_all_tasks)

let test_get_task _ =
  let task1 = Task.create ~title:"Task 1" ~date:date1 in
  let list_with_task1 = add_task task1 empty in
  assert_equal [ task1 ] (get_tasks date1 list_with_task1)

let test_get_no_task _ =
  let task1 = Task.create ~title:"Task 1" ~date:date1 in
  let list_with_task1 = add_task task1 empty in
  assert_equal [] (get_tasks date2 list_with_task1)

let suite =
  "TaskList Tests"
  >::: [
         "test_empty_task_list" >:: test_empty_task_list;
         "test_add_task_to_empty_list" >:: test_add_task_to_empty_list;
         "test_remove_task_one_ele" >:: test_remove_task_one_ele;
         "test_remove_task_two_ele" >:: test_remove_task_two_ele;
         "test_remove_task_absent" >:: test_remove_task_absent;
         "test_replace_task" >:: test_replace_task;
         "test_list_tasks" >:: test_list_tasks;
         "test_get_tasks" >:: test_get_task;
         "test_get_no_tasks" >:: test_get_no_task;
       ]

let () =
  run_test_tt_main suite;
  print_endline "test_tasklist passed all tests"
