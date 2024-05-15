(* event.mli *)
type t
(** The type representing an event. *)

type repeat_option =
  | NoRepeat
  | Daily
  | Weekly
  | Monthly
  | Yearly

type color_choices =
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Indigo
  | Violet

(** Represents whether an event repeats and how. *)

val create :
  id:int ->
  title:string ->
  description:string ->
  repeats:repeat_option ->
  color:color_choices ->
  t
(** [create ~id ~title ~description] creates a new event with the specified
    attributes.
    - [id] : the unique identifier of the event.
    - [title] : the title of the event.
    - [description] : the description of the event. *)

val get_title : t -> string
(** [get_title event] retrieves the title of [event]. *)

val get_description : t -> string
(** [get_description event] retrieves the description of [event]. *)

val get_repeats : t -> repeat_option
(** [get_repeats event] retrieves whether [event] repeats. *)
val get_color : t -> color_choices

val event_on_day : Date.t -> t -> bool
(** [event_on_day date event] is true if [event] can occur on [date]. *)

val edit : t -> title:string -> description:string -> color:color_choices -> t
(** [edit event ~title ~description ~date] returns a new event with the
    specified attributes updated.
    - [event] : the event to be edited.
    - [title] : the new title for the event.
    - [description] : the new description for the event. *)

val add_condition : (Date.t -> bool) -> t -> t
(** [add_condition condition event] makes it so that [event] has to adhear to
    [condition]. *)

val equals : t -> t -> bool
(** [equals e1 e2] returns true if the two events are equivilent; false
    otherwise. *)

val to_string : t -> string
(** [to_string event] converts the event [event] into a string representation.
    The string includes the ID, title, description, date, and repeat option of
    the event. *)

