(* taskList.mli *)

type t
(** The type representing the user's list of tasks *)

val empty : t
(** Returns an empty task list *)

val add_task : t -> Task.t -> t
(** [add_task] adds a task to the task list. *)

val remove_task : t -> string -> t
(** [remove_task] removes a task by its title from the task list *)

val edit_task : t -> string -> Task.t -> t
(** [edit_task] edits an existing task based on title in the task list *)

(* val get_task : t -> string -> Task.t * [get_task] gets a task based on a
   title and returns the task *)

val list_tasks : t -> string list
(** [list_tasks] lists all tasks in the calendar in string format*)

(* val compare_t_lists : t -> t -> (Task.t -> Task.t -> int) -> int (**
   [compare_lists] takes in two task lists [list1] and [list2]. For testing.
   Returns 1 if the tasklists aren't equal. Returns 0 if the tasklists are
   equal.*) *)
