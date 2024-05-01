open Bogue
open Main
module L = Layout
module W = Widget

(** The calendar we are displaying. *)
let cal = ref Calendar.empty

(** The current month. *)
let cur_month =
  ref
    (let today = Date.current_date () in
     MonthDisplay.get_month today.month today.year)

(** The current month layout. *)
let month_layout = ref (MonthDisplay.layout_of_month 100 !cal !cur_month)

(** Updates the display based on [cur_month] and [cal]. *)
let update_display () =
  let width = L.width !month_layout in
  let new_layout = MonthDisplay.layout_of_month (width / 7) !cal !cur_month in
  L.set_rooms !month_layout [ new_layout ];
  Sync.push (fun () ->
      print_endline ("Change to " ^ MonthDisplay.string_of_month !cur_month))

let next_btn =
  W.button ~kind:Button.Trigger
    ~label:(Label.create ~align:Draw.Center ">")
    ~action:(fun _ ->
      let m, y = MonthDisplay.get_month_info !cur_month in
      let m, y = Date.next_month m y in
      cur_month := MonthDisplay.get_month m y;
      Sync.push update_display)
    "Next month"

let prev_btn =
  W.button ~kind:Button.Trigger
    ~label:(Label.create ~align:Draw.Center "<")
    ~action:(fun _ ->
      let m, y = MonthDisplay.get_month_info !cur_month in
      let m, y = Date.prev_month m y in
      cur_month := MonthDisplay.get_month m y;
      Sync.push update_display)
    "Prev month"

let test () =
  let width = 1000 in
  let _ = update_display () in
  L.tower
    [
      !month_layout;
      L.resident (ButtonDisplay.button (width / 10));
      L.resident prev_btn;
      L.resident next_btn;
    ]
  |> of_layout |> run;
  quit ()
