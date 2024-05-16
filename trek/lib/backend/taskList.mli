(* taskList.mli *)

type t
(** The type representing the user's list of tasks *)

val empty : t
(** Returns an empty task list *)

val add_task : Task.t -> t -> t
(** [add_task task t_list] adds [task] to [t_list]. *)

val remove_task : Task.t -> t -> t
(** [remove_task task t_list] removes [task] from [t_list]. *)

val replace_task : Task.t -> Task.t -> t -> t
(** [replace_task task new_task t_list] replaces [task] with [new_task] in
    [t_list]. *)

val list_tasks : t -> string list
(** [list_tasks] lists all tasks in the calendar in string format*)

val get_tasks : Date.t -> t -> Task.t list
(** [get_tasks date t_list] returns all tasks that are due on [date]. *)
