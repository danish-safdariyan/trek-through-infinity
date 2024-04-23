module Event : sig
  type t

  type repeat_option =
    | NoRepeat
    | Daily
    | Weekly
    | Monthly
    | Yearly

  val create :
    id:int ->
    title:string ->
    description:string ->
    date:string ->
    repeats:repeat_option ->
    t

  val get_id : t -> int
  val get_title : t -> string

  (* Declare other getters if needed *)
  val edit :
    t ->
    title:string ->
    description:string ->
    date:string ->
    repeats:repeat_option ->
    t

  val to_string : t -> string
end
