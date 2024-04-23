open Trek
(* @author Gabriel Martinez-Amezaga (gm546), @author Kaz Tam (kt557), @author
   Danish Safdariyan (ds968), @author Andisha Safdariyan (as3254), @author
   Gabriella Best (gb486) *)

let () = print_endline "Hi! Please submit a name for your event!"
let user_event = read_line ()

let () =
  print_endline
    ("Now please select a day from Day 01 to Day 35 for your event to be on.\n"
   ^ "Please follow the format below: \n"
   ^ "\"Day 0X\" for single digit days, and then for double digit, \"Day XX\"")

let user_day = read_line ()

let _ =
  let default_event = "Nothing" (* Define your default event here *) in

  let calendar =
    let rec create_calendar acc day =
      if day <= 35 then
        let day_str = Printf.sprintf "%02d" day in
        let event_day =
          if user_day = "Day " ^ day_str then user_event else default_event
        in
        let acc' = CalDict.AssocListMap.insert day_str event_day acc in
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
