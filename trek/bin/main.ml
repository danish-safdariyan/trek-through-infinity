(* open Trek

   let () = print_endline "Hi! Please submit a name for your event:"; let
   user_event = read_line () in print_endline "Please submit a date in the form
   [month][day][year].\n\ For example, to submit the date March 27, 2024, you
   would type [03272024]"; let user_day = read_line () in let () = print_endline
   ("You have successfully added the event " ^ user_event ^ " on " ^ user_day ^
   " to your calendar.") in CalDict.insert user_day user_event *)
open Trek
open CalDict
open AssocListMap

let event_list = empty
let event_list = empty |> insert 1 "hello" |> insert 2 "hello" |> insert 3
