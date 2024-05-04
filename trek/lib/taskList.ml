(* taskList.ml (*Implementation of the taskList module for managing tasks*)

   module Map = CalDict.AssocListMap

   type t = (string, Task.t) Map.t (*need to fix*)

   let empty = Map.empty let add_task t_list task = failwith "TODO: Implement
   this function" let remove_task t_list task = failwith "TODO: Implement this
   function" let edit_task t_list task t_id = failwith "TODO: Implement this
   function" let get_task t_list t_id = failwith "TODO: Implement this function"
   let list_tasks t_list = failwith "TODO: Implement this function" *)
