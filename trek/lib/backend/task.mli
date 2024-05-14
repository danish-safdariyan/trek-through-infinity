(*type representing the task*)
type t

(**the type representing display options for tasks*)
type display_option =
  | CalDisplay
  | ListDisplay
  | BothDisplay

val create : title:string -> date:string -> display:display_option -> t
(** [create] creates a new task with the specified attribues - [id] : unique
    identifier for tasks - [title] : title of task to be displayed - [date] :
    the date that the task is due by in the format "YYYY-MM-DD" - [display] :
    the display option for the task *)

val get_title : t -> string
(**[get_title] gets the title of the task*)

val get_date : t -> string
(**[get_date] gets the date of the task*)

val get_display : t -> display_option
(**[get_display] gets the display option of the task. display_option can be any
   of the following:
   - CalDisplay: displays only on the calendar
   - ListDisplay: displays only on the task list
   - BothDisplay: displays on both the calendar and the task list*)

val edit_task : t -> title:string -> date:string -> display:display_option -> t
(** [edit_task] edits a task with the following updated attributes - [title] :
    title of task updated - [date] : the new due date for the task in the format
    "YYYY-MM-DD" - [display] : the updated display option for the task*)

val to_string : t -> string
(** [to_string] converst a task into a string representation of that task*)

val compare_tasks : t -> t -> int
(** [compare_tasks] compares two tasks [task1] and [task2]. For testing. Returns
    0 if they are the same. Returns 1 if they are different*)
