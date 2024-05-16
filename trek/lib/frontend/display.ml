open Bogue
open Main
open Backend
module L = Layout
module W = Widget
module P = Popups

(** The calendar we are displaying. *)
let cal = ref (Calendar.initialize_calendar Calendar.empty)

let taskList = ref TaskList.empty

(** The current month. *)
let cur_month =
  ref
    (let today = Date.current_date () in
     MonthDisplay.get_month today.month today.year)

let get_month () = MonthDisplay.get_month_info !cur_month

(** The current month layout. *)
let month_layout = L.empty ~w:1000 ~h:750 ()

let task_layout = L.empty ~w:270 ~h:445 ()

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
      let width = L.width month_layout in
      let height = L.height (L.top_house month_layout) in
      let new_layout =
        MonthDisplay.layout_of_month (width - 20) height !cal !cur_month
          update_calendar prev_btn nxt_btn
      in
      L.set_rooms month_layout [ new_layout ])

let update_calendar new_cal =
  cal := new_cal !cal;
  update_month_display ()

let add_event date title description repeats color =
  update_calendar (Calendar.add_event date title description repeats color)

let rec update_task_display () =
  let update_task_list new_task =
    taskList :=
      TaskList.add_task !taskList
        (Task.create ~title:new_task ~date:"00/00/00" ~display:ListDisplay);
    update_task_display ()
  in
  Sync.push (fun _ ->
      let new_layout =
        TaskListDisplay.taskListLayout 270
          (L.height month_layout - 305)
          (TaskList.list_tasks !taskList)
          update_task_list
      in
      L.set_rooms task_layout [ new_layout ])

let update_task_list new_task =
  taskList :=
    TaskList.add_task !taskList
      (Task.create ~title:new_task ~date:"00/00/00" ~display:ListDisplay);
  update_task_display ()

let rightScreenLayout eventLayout =
  let layout = L.tower [ eventLayout ] in
  (* let () = Space.full_height layout in *)
  L.superpose
    [ GenDisplay.theme_box (L.width layout) (L.height month_layout); layout ]

let test () =
  let new_event = EventDisplay.add_event_layout add_event in
  let _ =
    update_month_display ();
    update_task_display ()
  in
  let layout =
    L.flat
      [
        L.superpose [ GenDisplay.surrounding_box month_layout; month_layout ];
        rightScreenLayout new_event;
      ]
  in
  L.on_resize (L.top_house layout) (fun () -> update_month_display ());
  layout |> of_layout |> run;
  quit ()
