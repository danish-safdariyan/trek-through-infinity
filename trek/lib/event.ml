module Event = struct
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
  (* Add getters for other fields if necessary *)

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
end
