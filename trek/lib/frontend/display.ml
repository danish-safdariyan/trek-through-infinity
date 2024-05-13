open Bogue
open Main
open Backend

(* open GenDisplay *)
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
let month_layout = MonthDisplay.layout_of_month 100 !cal !cur_month

(** Updates the display based on [cur_month] and [cal]. *)
let update_display () =
  Sync.push (fun () ->
      let width = L.width month_layout in
      let new_layout =
        MonthDisplay.layout_of_month (width / 7) !cal !cur_month
      in
      L.set_rooms month_layout [ new_layout ])

let update_calendar c =
  cal := c;
  update_display ()

let update_month (m, y) =
  cur_month := MonthDisplay.get_month m y;
  update_display ()

let add_event date title description repeats =
  update_calendar (Calendar.add_event date title description repeats !cal)

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
  let new_event = AddEvent.add_event_layout add_event in
  let layout = L.tower [ menu; new_event; month_layout ] in
  L.on_resize (L.top_house layout) (fun () -> update_display ());
  layout |> of_layout |> run;
  quit ()
