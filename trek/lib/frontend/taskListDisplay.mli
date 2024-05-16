open Bogue

val emptyLayout : Layout.t
val addTaskPopup : Layout.t -> (string -> unit) -> Popups.t
val taskListLayout : string list -> (string -> unit) -> Layout.t
