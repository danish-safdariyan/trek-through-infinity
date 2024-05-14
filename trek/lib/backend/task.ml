type t = {
  title : string;
  date : string;
  display : display_option;
}

and display_option =
  | CalDisplay
  | ListDisplay
  | BothDisplay

let create ~title ~date ~display = { title; date; display }
let get_title task = task.title
let get_date task = task.date
let get_display task = task.display
let edit_task task ~title ~date ~display = { title; date; display }

let to_string task =
  Printf.sprintf "Title: %s, Date: %s, Display:\n   %s" task.title task.date
    (match task.display with
    | CalDisplay -> "Display on Calendar Only"
    | ListDisplay -> "Display on List Only"
    | BothDisplay -> "Display on Calendar and List")
