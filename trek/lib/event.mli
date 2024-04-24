module Event : sig
  type t
  (** The type representing an event. *)

  type repeat_option =
    | NoRepeat
    | Daily
    | Weekly
    | Monthly
    | Yearly

  (** The type representing repeat options for events. *)

  val create :
    id:int ->
    title:string ->
    description:string ->
    date:string ->
    repeats:repeat_option ->
    t

  (** [create ~id ~title ~description ~date ~repeats] creates a new event with
      the specified attributes.
      - [id] : the unique identifier of the event.
      - [title] : the title of the event.
      - [description] : the description of the event.
      - [date] : the date of the event in the format "YYYY-MM-DD".
      - [repeats] : the repeat option for the event. *)

  val get_id : t -> int
  (** [get_id event] retrieves the unique identifier of the event [event]. *)

  val get_title : t -> string
  (** [get_title event] retrieves the title of the event [event]. *)

  val edit :
    t ->
    title:string ->
    description:string ->
    date:string ->
    repeats:repeat_option ->
    t

  (** [edit event ~title ~description ~date ~repeats] returns a new event with
      the specified attributes updated.
      - [event] : the event to be edited.
      - [title] : the new title for the event.
      - [description] : the new description for the event.
      - [date] : the new date for the event in the format "YYYY-MM-DD".
      - [repeats] : the new repeat option for the event. *)

  val to_string : t -> string
  (** [to_string event] converts the event [event] into a string representation.
      The string includes the ID, title, description, date, and repeat option of
      the event. *)
end
