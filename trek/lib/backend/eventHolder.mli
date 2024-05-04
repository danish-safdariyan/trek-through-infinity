(* 
module EventHolder : sig 
  type ('k, 'v) t 

  (**[curr_day_cap] is the current capacity of [EventHolder]. For example, if 
      [EventHolder] has events on three different days, the [curr_day_cap] 
      would be 3. [curr_day_cap] will be increased after each resize of the
      [EventHolder]*)
  val curr_day_cap: int

  (**[lim_event_cap] is the chosen capacity for events a user can input per day.
      For example, it does not make sense for a day to have 20+ events. The capacity
      is immutable. If the user tries to input more than [lim_event_cap] for a 
      specific day, they will be asked to delete an event*)
  val lim_event_cap: int

  (**[find] takes in a key [k] and produces either the [v] associated with 
      that key or None if the key does not already exist*)
  val find: 'k -> ('k, 'v) t -> 'v option

  (**[insert_no_resize] takes in a key [k] and event [e] and adds [e] to [v]
      associated with that [k] if it already exists within the [EventHolder]*)
  val insert_no_resize: 'k -> 'e -> ('k, 'v) t -> unit
  
  (**[insert_resize] takes in a key [k] that is not already in the [EventHolder]
  and event [e] and produces a new [EventHolder] with that new [k]*)
  val insert_resize: 'k -> 'e -> ('k, 'v) t -> ('k, 'v) t

  (**[remove] takes in a key [k] and event [e] and removes [e] from the event array
      [v] associated with [k]. If [e] was the only event in [v], [k] still remains
      in the [EventHolder]. If there is no key [k] in the [EventHolder], the 
      user is prompted to try again with a valid key. If there is no event [e] in
      [k], the user is prompted to try again with a valid [e]*)
  val remove: 'k -> 'e -> ('k, 'v) t 
end
 *)
