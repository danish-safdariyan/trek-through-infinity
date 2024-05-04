(* type t = { id : int; title : string; date : string; display : display_option;
   }

   and display_option = | CalDisplay | ListDisplay | BothDisplay

   let create ~id ~title ~date ~display = { id; title; date; display } let
   get_id task = task.id let get_title task = task.title let edit_task task
   ~title ~date ~display = { task with title; date; display }

   let to_string task = Printf.sprintf "ID: %d, Title: %s, Date: %s, Display:
   %s" task.id task.title task.date (match task.display with | CalDisplay ->
   "Display on Calendar Only" | ListDisplay -> "Display on List Only" |
   BothDisplay -> "Display on Calendar and List") *)
