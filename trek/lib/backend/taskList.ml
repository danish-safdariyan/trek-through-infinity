(* taskList.ml  *)
(*Implementation of the taskList module for managing tasks*)
open Task
module Map = CalDict.AssocListMap

type t = (Date.t, Task.t list) Map.t

let empty = Map.empty

let add_task task t_list =
  let date = Task.get_date task in
  try
    let existing_tasks = Map.lookup date t_list in
    Map.insert date (task :: existing_tasks) t_list
  with Not_found -> Map.insert date [ task ] t_list

let remove_task task t_list =
  let tasks =
    try Map.lookup (Task.get_date task) t_list with Not_found -> []
  in
  let filtered_tasks = List.filter (fun t -> Task.equals t task |> not) tasks in
  Map.insert (Task.get_date task) filtered_tasks t_list

let replace_task task new_task t_list =
  remove_task task t_list |> add_task new_task

let rec list_helper bindings string_list =
  match bindings with
  | [] -> string_list
  | (_, v) :: t -> list_helper t (string_list @ List.map to_string v)

let get_tasks date t_list = try Map.lookup date t_list with Not_found -> []
let list_tasks t_list = list_helper (Map.bindings t_list) []
