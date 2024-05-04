taskList.mli *)
(** Interface for the taskList module, managing a collection of tasks *)

type t
(** The type representing the user's list of tasks *)

val empty : t
(** Returns an empty task list *)

val add_task : t -> Task.t -> t
(** [add_task] adds a task to the task list. The tasks with the closest
    deadlines will be placed at the top of the task list*)

val remove_task : t -> Task.t -> int -> t
(** [remove_task] removes a task by its ID from the task list *)

val edit_task : t -> Task.t -> int -> t
(** [edit_task] edits an existing task in the task list *)

val get_task : t -> int -> Task.t
(** [get_task] gets a task based on an ID and returns the task *)

val list_tasks : t -> string list
(** [list_tasks] lists all tasks in the calendar in string format
