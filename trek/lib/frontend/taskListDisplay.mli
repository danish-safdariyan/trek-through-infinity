open Bogue
open Backend

val left_side_layout :
  int ->
  int ->
  Layout.t ->
  TaskList.t ->
  ((TaskList.t -> TaskList.t) -> unit) ->
  Layout.t
(** [left_side_layout w h new_event task_lst update_task_lst] creates the left
    side of the display. [w] is width, [h] is height, [new_event] is the top
    part of the left side, [task_lst] is the task list, [update_task_lst] is a
    function that allows it to update the task list. *)
