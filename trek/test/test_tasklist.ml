open OUnit2
open Backend
(* open Task open TaskList *)

let test_empty_task_list _ =
  let empty_list = TaskList.empty in
  assert_equal empty_list TaskList.empty

let test_add_task_to_empty_list _ =
  let task1 =
    Task.create ~title:"Task 1" ~date:"2024-05-13" ~display:CalDisplay
  in
  let list_with_task1 = TaskList.add_task TaskList.empty task1 in
  assert_equal list_with_task1 (TaskList.add_task TaskList.empty task1)

let test_add_task_with_same_title _ =
  let task1 =
    Task.create ~title:"Task 1" ~date:"2024-05-13" ~display:CalDisplay
  in
  let list_with_task1 = TaskList.add_task TaskList.empty task1 in
  assert_equal list_with_task1 (TaskList.add_task list_with_task1 task1)

let test_remove_task _ =
  let task1 =
    Task.create ~title:"Task 1" ~date:"2024-05-13" ~display:CalDisplay
  in
  let list_with_task1 = TaskList.add_task TaskList.empty task1 in
  let list_without_task1 = TaskList.remove_task list_with_task1 "Task 1" in
  assert_equal TaskList.empty list_without_task1

let test_edit_task _ =
  let task1 =
    Task.create ~title:"Task 1" ~date:"2024-05-13" ~display:CalDisplay
  in
  let list_with_task1 = TaskList.add_task TaskList.empty task1 in
  let edited_task =
    Task.edit_task task1 ~title:"Updated Task" ~date:"2024-05-14"
      ~display:ListDisplay
  in
  let updated_list = TaskList.edit_task list_with_task1 "Task 1" edited_task in
  assert_equal updated_list (TaskList.add_task TaskList.empty edited_task)

(* let test_list_tasks _ = let task1 = Task.create ~title:"Task 1"
   ~date:"2024-05-13" ~display:CalDisplay in let task2 = Task.create
   ~title:"Task 2" ~date:"2024-05-14" ~display:ListDisplay in let
   list_with_tasks = TaskList.add_task TaskList.empty task1 in let
   list_with_all_tasks = TaskList.add_task list_with_tasks task2 in assert_equal
   [ "Title: Task 1, Date: 2024-05-13, Display:\n Display on Calendar Only";
   "Title: Task 2, Date: 2024-05-14, Display:\n Display on List Only"; ]
   (TaskList.list_tasks list_with_all_tasks) *)

let test_list_tasks _ =
  let task1 =
    Task.create ~title:"Task 1" ~date:"2024-05-13" ~display:CalDisplay
  in
  let task2 =
    Task.create ~title:"Task 2" ~date:"2024-05-14" ~display:ListDisplay
  in
  let list_with_tasks = TaskList.add_task TaskList.empty task1 in
  let list_with_all_tasks = TaskList.add_task list_with_tasks task2 in
  let actual = TaskList.list_tasks list_with_all_tasks in
  let expected =
    [
      "Title: Task 1, Date: 2024-05-13, Display:\n Display on Calendar Only";
      "Title: Task 2, Date: 2024-05-14, Display:\n Display on List Only";
    ]
  in
  List.iter2
    (fun a e -> Printf.printf "Expected: %s\nActual: %s\n" e a)
    actual expected;
  assert_equal expected actual

let suite =
  "TaskList Tests"
  >::: [
         "test_empty_task_list" >:: test_empty_task_list;
         "test_add_task_to_empty_list" >:: test_add_task_to_empty_list;
         "test_add_task_with_same_title" >:: test_add_task_with_same_title;
         "test_remove_task" >:: test_remove_task;
         "test_edit_task" >:: test_edit_task;
         "test_list_tasks" >:: test_list_tasks;
       ]

let () =
  run_test_tt_main suite;
  print_endline "test_tasklist passed all tests"
