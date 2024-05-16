(*type representing the task*)
type t

val create : title:string -> date:Date.t -> t
(** [create] creates a new task with the specified attribues - [id] : unique
    identifier for tasks - [title] : title of task to be displayed - [date] :
    the date that the task is due by in the format "YYYY-MM-DD" - [display] :
    the display option for the task *)

val get_title : t -> string
(**[get_title] gets the title of the task*)

val get_date : t -> Date.t
(**[get_date] gets the date of the task*)

val edit_task : t -> title:string -> date:Date.t -> t
(** [edit_task] edits a task with the following updated attributes - [title] :
    title of task updated - [date] : the new due date for the task in the format
    "YYYY-MM-DD" *)

val to_string : t -> string
(** [to_string] converst a task into a string representation of that task*)

val equals : t -> t -> bool
(** [equals] compares two tasks [task1] and [task2]. Returns [true] if they are
    the same. Returns [false] if they are different*)
