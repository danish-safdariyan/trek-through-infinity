open Bogue
open Main
open Backend
module L = Layout
module W = Widget
module P = Popups

(** The calendar we are displaying. *)
let cal = ref (Calendar.initialize_calendar Calendar.empty)

(** Current task list that is being displayed *)
let taskList = ref TaskList.empty

(** The current month. *)
let cur_month =
  ref
    (let today = Date.current_date () in
     MonthDisplay.get_month today.month today.year)

(** Gets current month. *)
let get_month () = MonthDisplay.get_month_info !cur_month

(** Width of left side items. *)
let side_width = 270

(** The current month layout. *)
let month_layout = L.empty ~w:1000 ~h:750 ()

(* let task_layout = L.empty ~w:side_width ~h:445 () *)
let left_layout = L.empty ~w:(side_width + 20) ~h:750 ()

(** Updates the display based on [cur_month] and [cal]. *)
let rec update_month_display () =
  let update_month (m, y) =
    cur_month := MonthDisplay.get_month m y;
    update_month_display ()
  in
  let prev_btn = L.resident (ButtonDisplay.prev_btn get_month update_month) in
  let nxt_btn = L.resident (ButtonDisplay.next_btn get_month update_month) in
  let update_calendar new_cal =
    cal := new_cal !cal;
    update_month_display ()
  in
  Sync.push (fun () ->
      let width = L.width (L.top_house month_layout) - side_width - 60 in
      let height = L.height (L.top_house month_layout) in
      let new_layout =
        MonthDisplay.layout_of_month (width - 20) height !cal !cur_month
          update_calendar prev_btn nxt_btn
      in
      L.set_rooms month_layout [ new_layout ])

(** Updates calendar with the function provided. *)
let update_calendar new_cal =
  cal := new_cal !cal;
  update_month_display ()

(** Layout for adding new events *)
let new_event = EventDisplay.add_event_layout update_calendar

(** Updates the task display *)
let rec update_task_display () =
  let update_task_list new_task_list =
    taskList := new_task_list !taskList;
    update_task_display ()
  in
  Sync.push (fun _ ->
      let height = L.height (L.top_house left_layout) - 20 in
      let new_layout =
        TaskListDisplay.left_side_layout side_width height new_event !taskList
          update_task_list
      in
      L.set_rooms left_layout [ new_layout ])

let calendar_display () =
  let _ =
    update_month_display ();
    update_task_display ();
    L.setx month_layout (side_width + 40)
  in
  let layout = L.flat [ left_layout; month_layout ] in
  let top = L.top_house layout in
  L.on_resize top (fun () ->
      update_month_display ();
      update_task_display ();
      L.setx month_layout (side_width + 40));
  layout |> of_layout |> run;
  quit ()
