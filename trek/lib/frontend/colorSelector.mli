open Backend
open Bogue

type t
(** Represents a color selector. *)

val make_selector : unit -> t
(** Creates a color selector. *)

val get_color : t -> Event.color_choices
(** Gets the color currently selected. *)

val get_layout : t -> Layout.t
(** Gets the layout of the selector. *)

val set_color : Event.color_choices -> t -> unit
(** Sets the color of the selector. *)
