type t = {
  id : int;
  title : string;
  description : string;
}

let create ~id ~title ~description = { id; title; description }
let get_title event = event.title

let edit event ?(title = event.title) ?(description = event.description) =
  { event with title; description }

let to_string event =
  Printf.sprintf "ID: %d, Title: %s, Description: %s" event.id event.title
    event.description

let equals e1 e2 = e1.id = e2.id
