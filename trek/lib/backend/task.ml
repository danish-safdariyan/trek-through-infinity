type t = {
  title : string;
  date : Date.t;
}

let create ~title ~date = { title; date }
let get_title task = task.title
let get_date task = task.date
let edit_task task ~title ~date = { title; date }

let to_string task =
  Printf.sprintf "Title: %s, Date: %s" task.title (Date.format_date task.date)

let equals task1 task2 =
  let title_compare = String.compare (get_title task1) (get_title task2) in
  let date_compare = Date.compare (get_date task1) (get_date task2) in
  match (title_compare, date_compare) with
  | 0, 0 -> true
  | _ -> false (* Different titles or dates *)
