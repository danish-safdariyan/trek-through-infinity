
(*type representing the task*)
type t

(**the type representing display options for tasks*)
type display_option =
|CalDisplay
|ListDisplay
|BothDisplay

(** [create] creates a new task with the specified attribues
    - [id] : unique identifier for tasks
    - [title] : title of task to be displayed
    - [date] : the date that the task is due by in the format "YYYY-MM-DD"
    - [display] : the display option for the task
    *)
val create :
  id: int ->
  title: string ->
  date: string -> 
  display: display_option ->
  t

(** [get_id] gets the identifier for the task*)
val get_id : t -> int

(**[get_title] gets the title of the task*)
val get_title : t -> string

(** [edit_task] edits a task with the following updated attributes
    - [title] : title of task updated
    - [date] : the new due date for the task in the format "YYYY-MM-DD"
    - [display] : the updated display option for the task
    *)
val edit_task :
  t ->
  title: string ->
  date: string -> 
  display: display_option ->
  t

(** [to_string] converst a task into a string representation of that task*)
val to_string: t -> string



