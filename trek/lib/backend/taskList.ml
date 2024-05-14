(* taskList.ml  *)
(*Implementation of the taskList module for managing tasks*)
open Task
module Map = CalDict.AssocListMap

type t = (string, Task.t) Map.t

let empty = Map.empty

let add_task t_list task =
  let title = Task.get_title task in
  if Map.lookup title t_list <> task then Map.insert title task t_list
  else t_list

let remove_task t_list title = Map.remove title t_list

let edit_task t_list title updated_task =
  add_task (remove_task t_list title) updated_task
(*removes task with that title form the list and then adds the updated task*)

(* let get_task t_list title = Map.lookup title t_list *)

let rec list_helper bindings string_list =
  match bindings with
  | [] -> string_list
  | (k, v) :: t -> list_helper t (string_list @ [ to_string v ])

let list_tasks t_list = list_helper (Map.bindings t_list) []

(* let compare_t_lists t_list1 t_list2 compare_tasks = Map.compare_lists t_list1
   t_list2 compare_tasks *)
