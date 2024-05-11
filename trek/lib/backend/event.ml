(* type t = { id : int; title : string; description : string; date : string; (*
   Format: "YYYY-MM-DD" *) repeats : repeat_option; time : string; }

   and repeat_option = | NoRepeat | Daily | Weekly | Monthly | Yearly

   let create ~id ~title ~description ~date ~repeats ~time = { id; title;
   description; date; repeats; time }

   let get_id event = event.id let get_title event = event.title

   let edit event ~title ~description ~date ~repeats ~time = { event with title;
   description; date; repeats; time }

   let to_string event = Printf.sprintf "ID: %d, Title: %s, Description: %s,
   Date: %s, Repeats: %s, Time: %s" event.id event.title event.description
   event.date (match event.repeats with | NoRepeat -> "No Repeat" | Daily ->
   "Daily" | Weekly -> "Weekly" | Monthly -> "Monthly" | Yearly -> "Yearly")
   event.time *)

type t = {
  id : int;
  title : string;
  description : string;
  date : string; (* Format: "YYYY-MM-DD" *)
  repeats : repeat_option;
}

and repeat_option =
  | NoRepeat
  | Daily
  | Weekly
  | Monthly
  | Yearly

let create ~id ~title ~description ~date ~repeats =
  { id; title; description; date; repeats }

let get_id event = event.id
let get_title event = event.title
let get_repeats event = event.repeats

let edit event ~title ~description ~date ~repeats =
  { event with title; description; date; repeats }

let to_string event =
  Printf.sprintf "ID: %d, Title: %s, Description: %s, Date: %s, Repeats: %s"
    event.id event.title event.description event.date
    (match event.repeats with
    | NoRepeat -> "No Repeat"
    | Daily -> "Daily"
    | Weekly -> "Weekly"
    | Monthly -> "Monthly"
    | Yearly -> "Yearly")
