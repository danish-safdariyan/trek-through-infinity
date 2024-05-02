open Bogue

type t

val attach_popup : Layout.t -> Layout.t -> t
(** [attach_popup layout popup] attaches [popup] to [layout] and returns a value
    with type [t] representing the popup.*)

val get_layout : t -> Layout.t
(** [get_layout p] returns the popup layout represented by [p]. *)

val toggle_popup : t -> unit
(** Toggles the state of the popup. *)

val show_popup : t -> unit
(** Shows the popup. *)

val hide_popup : t -> unit
(** Hides the popup. *)
