open Bogue
open Backend

val left_side_layout :
  int -> int -> Layout.t -> TaskList.t -> (string -> string -> unit) -> Layout.t
