open Bogue

val addTaskPopup : Layout.t -> (string -> unit) -> Popups.t
val taskListLayout : int -> int -> string list -> (string -> unit) -> Layout.t
