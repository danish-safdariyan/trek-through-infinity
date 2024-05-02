open Bogue
open Main
module L = Layout
module W = Widget
module P = Popups

(** The calendar we are displaying. *)
let cal = ref Calendar.empty

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

let get_month () = MonthDisplay.get_month_info !cur_month

let test () =
  let width = 1000 in
  let _ = update_display () in
  let btn = ButtonDisplay.button (width / 10) in
  let menu =
    L.flat
      [
        L.resident (ButtonDisplay.prev_btn get_month update_month);
        L.resident (ButtonDisplay.next_btn get_month update_month);
        L.resident btn;
      ]
  in
  let layout = L.tower [ menu; month_layout ] in
  let close_event_popup_btn = W.button "OK" in
  let add_event_popup =
    P.attach_popup layout
      (let label = W.label "This is a popup" in
       L.tower_of_w [ label; close_event_popup_btn ])
  in
  W.on_button_release
    ~release:(fun _ -> P.hide_popup add_event_popup)
    close_event_popup_btn;
  W.on_button_release ~release:(fun _ -> P.toggle_popup add_event_popup) btn;
  layout |> of_layout |> run;
  quit ()
