open Bogue
open Backend

val left_side_layout :
  int ->
  int ->
  Layout.t ->
  TaskList.t ->
  ((TaskList.t -> TaskList.t) -> unit) ->
  Layout.t
