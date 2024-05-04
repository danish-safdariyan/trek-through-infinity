open Bogue

type t

val attach_popup : ?bg:Draw.color -> Layout.t -> Layout.t -> t
(** [attach_popup layout popup] attaches [popup] to [layout] and returns a value
    with type [t] representing the popup.*)

val get_layout : t -> Layout.t
(** [get_layout p] returns the popup layout represented by [p]. *)

val toggle : t -> unit
(** Toggles the state of the popup. *)

val show : t -> unit
(** Shows the popup. *)

val hide : t -> unit
(** Hides the popup. *)

val get_state : t -> bool
(** [get_state popup] is whether [popup] is shown. *)

val should_exit_on_press : t -> bool -> unit
(** [should_exit_on_press popup true] means the popup automatically closes if
    the user clicks anywhere other than the popup. Default: [false]. *)
