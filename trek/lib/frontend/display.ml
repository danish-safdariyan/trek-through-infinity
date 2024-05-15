open Bogue
open Main
open Backend
module L = Layout
module W = Widget
module P = Popups

(** The calendar we are displaying. *)
let cal = ref (Calendar.initialize_calendar Calendar.empty)

(** The current month. *)
let cur_month =
  ref
    (let today = Date.current_date () in
     MonthDisplay.get_month today.month today.year)

(** The current month layout. *)
let month_layout = L.empty ~w:1000 ~h:1000 ()

(** Updates the display based on [cur_month] and [cal]. *)
let rec update_display () =
  let update_calendar new_cal =
    cal := new_cal !cal;
    update_display ()
  in
  Sync.push (fun () ->
      let width = L.width month_layout in
      let new_layout =
        MonthDisplay.layout_of_month (width / 7) !cal !cur_month update_calendar
      in
      L.set_rooms month_layout [ new_layout ])

let update_calendar new_cal =
  cal := new_cal !cal;
  update_display ()

let update_month (m, y) =
  cur_month := MonthDisplay.get_month m y;
  update_display ()

let add_event date title description repeats color =
  update_calendar (Calendar.add_event date title description repeats color)

let get_month () = MonthDisplay.get_month_info !cur_month

let test () =
  let width = 1000 in
  let _ = update_display () in
  let add_event_btn = ButtonDisplay.button (width / 10) in
  let menu =
    L.flat
      [
        L.resident (ButtonDisplay.prev_btn get_month update_month);
        L.resident (ButtonDisplay.next_btn get_month update_month);
        L.resident add_event_btn;
        Space.vfill ();
        Space.hfill ();
      ]
  in
  let new_event = EventDisplay.add_event_layout add_event in
  let layout = L.tower [ menu; new_event; month_layout ] in
  L.on_resize (L.top_house layout) (fun () -> update_display ());
  layout |> of_layout |> run;
  quit ()
