type t
(** The type representing an event. *)

val create : id:int -> title:string -> description:string -> t
(** [create ~id ~title ~description] creates a new event with the specified
    attributes.
    - [id] : the unique identifier of the event.
    - [title] : the title of the event.
    - [description] : the description of the event. *)

val get_title : t -> string
(** [get_title event] retrieves the title of the event [event]. *)

val edit : t -> ?title:string -> ?description:string -> t
(** [edit event ~title ~description ~date] returns a new event with the
    specified attributes updated.
    - [event] : the event to be edited.
    - [title] : the new title for the event.
    - [description] : the new description for the event. *)

val equals : t -> t -> bool
(** [equals e1 e2] returns true if the two events are equivilent; false
    otherwise. *)

val to_string : t -> string
(** [to_string event] converts the event [event] into a string representation.
    The string includes the ID, title, description, date, and repeat option of
    the event. *)
