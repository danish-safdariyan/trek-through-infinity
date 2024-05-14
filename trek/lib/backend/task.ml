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

let compare_tasks task1 task2 =
  let title_compare = String.compare (get_title task1) (get_title task2) in
  let date_compare = String.compare (get_date task1) (get_date task2) in
  match (title_compare, date_compare) with
  | 0, 0 -> (
      match (get_display task1, get_display task2) with
      | CalDisplay, CalDisplay -> 0
      | ListDisplay, ListDisplay -> 0
      | BothDisplay, BothDisplay -> 0
      | _ -> 1 (* Different display options *))
  | _ -> 1 (* Different titles or dates *)
