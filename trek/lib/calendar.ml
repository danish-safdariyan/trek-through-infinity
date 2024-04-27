(* calendar.ml *)
(** Implementation of the Calendar module for managing events *)

module Map = CalDict.AssocListMap

type t = (Date.t, Event.t list) Map.t

let empty = Map.empty


let add_event calendar date event =
  (*returns empty list if date is not found*)
  let events = try Map.lookup date calendar with Not_found -> [] in
  (*if date is found, add event to the list of events for that day*)
  Map.insert date (event :: events) calendar


let remove_event calendar date event_id =
  (*if day is not in calendar, return empty list*)
  let events = try Map.lookup date calendar with Not_found -> [] in
  let filtered_events =
    (*if the day is in the calendar, filter through the events and return the 
       new list without the event id given*)
    List.filter (fun e -> Event.get_id e <> event_id) events
  in
  (**)
  Map.insert date filtered_events calendar

let edit_event calendar date event_id updated_event =
  let events = try Map.lookup date calendar with Not_found -> [] in
  let updated_events =
    List.map
      (fun e -> if Event.get_id e = event_id then updated_event else e)
      events
  in
  Map.insert date updated_events calendar

let find_events calendar date =
  try Map.lookup date calendar with Not_found -> []

let list_all_events calendar =
  Map.bindings calendar
  |> List.map (fun (_, events) -> List.map Event.to_string events)
     (* Convert each event to string *)
  |> List.flatten
(* Flatten the list of lists of strings into a list of strings *)
