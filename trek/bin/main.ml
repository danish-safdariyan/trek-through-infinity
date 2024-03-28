open Trek

let () = print_endline "Hi! Please submit a name for your event!"
let user_event = read_line ()

let () =
  print_endline
    "Please submit a date in the form\n\
    \   [month][day][year]. \n\
     For example, to submit the date March 27, 2024, you\n\
    \   would type [03272024]"

let user_day = read_line ()

let () =
  print_endline
    ("You have successfully added the event " ^ user_event ^ " on " ^ user_day
   ^ " to your calendar.")
