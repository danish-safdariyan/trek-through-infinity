open Trek

(* let () = print_endline "Hi! Please submit a name for your event!" let
   user_event = read_line ()

   let () = print_endline "Please submit a date in the form\n\ \
   [month][day][year]. \n\ For example, to submit the date March 27, 2024,
   you\n\ \ would type [03272024]"

   let user_day = read_line ()

   let () = print_endline ("You have successfully added the event " ^ user_event
   ^ " on " ^ user_day ^ " to your calendar.") *)

(* let _ = Example.example13 () *)

let _ =
  let default_event = "Nothing" (* Define your default event here *) in

  let calendar =
    let rec create_calendar acc day =
      if day <= 35 then
        let day_str = Printf.sprintf "%02d" day in
        let acc' = CalDict.AssocListMap.insert day_str default_event acc in
        create_calendar acc' (day + 1)
      else acc
    in
    create_calendar CalDict.AssocListMap.empty 1
  in

  let print_calendar () =
    List.map
      (fun (day, event) -> Printf.sprintf "Day %s: %s" day event)
      (CalDict.AssocListMap.bindings calendar)
  in

  let calendar = print_calendar () in

  Display.test calendar
