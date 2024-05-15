open Bogue
open Backend

val addTaskPopup : Layout.t -> (string -> unit) -> Popups.t
val taskListLayout : string list -> (string -> unit) -> Layout.t
