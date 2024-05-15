(* event.ml *)

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

type t = {
  id : int;
  title : string;
  description : string;
  repeats : repeat_option;
  color : color_choices;
  special_condition : Date.t -> bool;
}

let create ~id ~title ~description ~repeats ~color =
  {
    id;
    title;
    description;
    repeats;
    color;
    special_condition = (fun _ -> true);
  }

let get_title event = event.title
let get_description event = event.description
let get_repeats event = event.repeats
let get_color event = event.color

let edit event ~title ~description ~color =
  { event with title; description; color }

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
