(* taskList.ml  *)
(*Implementation of the taskList module for managing tasks*)
open Task
module Map = CalDict.AssocListMap

type t = (string, Task.t) Map.t (*need to fix*)

let empty = Map.empty

let add_task t_list task =
  let tasks = try Map.lookup (get_title task) t_list with Not_found -> [] in
  Map.insert (get_title task) (task :: tasks) t_list

let remove_task t_list task =
  let tasks = try Map.lookup (get_title task) t_list with Not_found -> [] in
  let filtered_tasks =
    List.filter (fun e -> get_title e <> get_title task) tasks
  in
  Map.insert (get_title task) filtered_tasks t_list

let edit_task t_list title updated_task =
  let tasks = try Map.lookup title t_list with Not_found -> [] in
  let updated_tasks =
    List.map (fun e -> if get_title e = title then updated_task else e) tasks
  in
  Map.insert title updated_tasks t_list

let get_task t_list title = try Map.lookup title t_list with Not_found -> []

let list_tasks t_list =
  Map.bindings t_list
  |> List.map (fun (_, tasks) -> List.map Event.to_string tasks)
  |> List.flatten
(* Flatten the list of lists of strings into a list of strings *)
