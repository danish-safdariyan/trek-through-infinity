type time = {
  hour : int; (* 1 to 12 *)
  minute : int; (* 0 to 59 *)
  am_pm : string;
}

type time_range = {
  start_time : time;
  end_time : time;
}

let validate_time_range start_time end_time =
  if
    start_time.hour < 1 || start_time.hour > 12 || start_time.minute < 0
    || start_time.minute > 59 || end_time.hour < 1 || end_time.hour > 12
    || end_time.minute < 0 || end_time.minute > 59
  then
    failwith
      "Invalid time range. Hours should be between 1 and 12, and minutes \
       between 0 and 59."
  else if
    (start_time.am_pm = "AM" && end_time.am_pm = "PM")
    || (start_time.am_pm = end_time.am_pm && start_time.hour > end_time.hour)
    || start_time.am_pm = end_time.am_pm
       && start_time.hour = end_time.hour
       && start_time.minute >= end_time.minute
  then failwith "End time must be after start time."
  else ()

(* let check_time_conflict event1 event2 = match (event1.time_range,
   event2.time_range) with | Some tr1, Some tr2 -> let start1 = tr1.start_time
   in let end1 = tr1.end_time in let start2 = tr2.start_time in let end2 =
   tr2.end_time in start1.am_pm = start2.am_pm && start1.hour = start2.hour &&
   start1.minute = start2.minute || end1.am_pm = end2.am_pm && end1.hour =
   end2.hour && end1.minute = end2.minute || start1.am_pm = start2.am_pm &&
   start1.hour = start2.hour && start1.minute < start2.minute && end1.am_pm =
   end2.am_pm && end1.hour = end2.hour && end1.minute > start2.minute ||
   start1.am_pm = start2.am_pm && start1.hour < start2.hour && end1.am_pm =
   start2.am_pm && end1.hour > start2.hour || start1.am_pm = start2.am_pm &&
   start1.hour = start2.hour && start1.minute > start2.minute && end1.am_pm =
   start2.am_pm && end1.hour > start2.hour || start1.am_pm = start2.am_pm &&
   start1.hour > start2.hour && end1.am_pm = start2.am_pm && end1.hour =
   start2.hour && end1.minute < start2.minute | _ -> false *)
let get_start_time event_time = event_time.start_time
let end_start_time event_time = event_time.end_time
