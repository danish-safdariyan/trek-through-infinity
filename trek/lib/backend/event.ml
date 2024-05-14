type repeat_option =
  | NoRepeat
  | Daily
  | Weekly
  | Monthly
  | Yearly

type t = {
  id : int;
  title : string;
  description : string;
  repeats : repeat_option;
  special_condition : Date.t -> bool;
}

let create ~id ~title ~description ~repeats =
  { id; title; description; repeats; special_condition = (fun _ -> true) }

let get_title event = event.title
let get_description event = event.description
let get_repeats event = event.repeats
let edit event ~title ~description = { event with title; description }

let to_string event =
  Printf.sprintf "ID: %d, Title: %s, Description: %s" event.id event.title
    event.description

let equals e1 e2 = e1.id = e2.id

let add_condition condition event =
  {
    event with
    special_condition = (fun d -> event.special_condition d && condition d);
  }

let event_on_day date event = event.special_condition date
