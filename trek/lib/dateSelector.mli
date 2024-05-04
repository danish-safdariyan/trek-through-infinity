open Bogue

type t

val make_selector : unit -> t
(** Creates a date selector *)

val get_date : t -> Date.t
(** Gets the date the selector currently has selected. *)

val get_layout : t -> Layout.t
(** Gets the layout of the selector. *)

val on_update : t -> (Date.t -> unit) -> unit
(** [on_update sel f] sets [f] as the behavior that occurs when [sel] updates. *)

val set_date : t -> Date.t -> unit
(** [set_date sel d] sets the selector to [d] and updates the display to that
    month/year. Runs the [on_update] function. *)
