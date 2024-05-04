(** [time] Type representing the time. *)
type time = {
  hour : int; (* 1 to 12 *)
  minute : int; (* 0 to 59 *)
  am_pm : string;
}
(** [time_range] Type representing the event time. *)
type time_range = {
  start_time : time;
  end_time : time;
}

val validate_time_range : time -> time -> unit

(* val validate_event_time : 'a -> unit

val check_time_conflict : 'a -> 'a -> bool *)

(** [get_start_time] gets the start time. *)
val get_start_time : time_range -> time 


(** [get_end_time] gets the end time. *)
val get_end_time : time_range -> time

