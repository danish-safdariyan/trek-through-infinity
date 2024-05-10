open Bogue
open Main
open Backend
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

(** Attaches the popup for adding new events to [layout]; all code for the popup
    of making new events should be put here. *)
let attach_new_event_popup layout =
  let title_input = W.text_input ~prompt:"Title                 " () in
  let desc_input = W.text_input ~prompt:"Description           " () in
  let rep_input = Select.create [| "Once"; "Weekly"; "Monthly"; "Yearly" |] 0 in
  let date_input = W.button (Date.current_date () |> Date.format_date) in
  let create_btn = W.button "OK" in
  let cancel_btn = W.button "Cancel" in
  let buttons = L.flat_of_w [ create_btn; cancel_btn ] in
  let stuff =
    L.tower
      [
        W.label ~size:15 "New Event:" |> L.resident;
        title_input |> L.resident;
        date_input |> L.resident;
        desc_input |> L.resident;
        rep_input;
        buttons;
      ]
  in
  let box =
    W.box ~w:(L.width stuff) ~h:(L.height stuff)
      ~style:
        (Style.create
           ~background:(Style.color_bg (Draw.opaque Draw.white))
           ~border:
             (Style.mk_line ~color:(Draw.opaque Draw.dark_grey) ~width:2 ()
             |> Style.mk_border)
           ())
      ()
    |> L.resident
  in
  let popup =
    P.attach_popup
      ~bg:(Draw.transp Draw.dark_grey)
      layout
      (L.superpose [ box; stuff ])
  in
  let selector = DateSelector.make_selector () in
  let date_selector_popup =
    P.attach_popup layout (DateSelector.get_layout selector)
  in
  let on_close () =
    P.hide popup;
    P.hide date_selector_popup;
    W.set_text title_input "";
    W.set_text desc_input "";
    DateSelector.set_date selector (Date.current_date ())
  in
  let _ =
    P.should_exit_on_press date_selector_popup true;
    W.on_button_release ~release:(fun _ -> on_close ()) cancel_btn;
    W.on_button_release
      ~release:(fun _ ->
        let event =
          Event.create ~id:1
            ~title:(W.get_text_input title_input |> Text_input.text)
            ~description:(W.get_text_input desc_input |> Text_input.text)
            ~date:"???" ~repeats:Event.Daily
        in
        update_calendar
          (Calendar.add_event !cal (DateSelector.get_date selector) event);
        on_close ())
      create_btn;
    DateSelector.on_update selector (fun _ ->
        W.set_text date_input
          (DateSelector.get_date selector |> Date.format_date);
        P.hide date_selector_popup);
    W.on_button_release
      ~release:(fun _ ->
        if P.get_state date_selector_popup then (
          P.hide date_selector_popup;
          DateSelector.get_date selector |> DateSelector.set_date selector)
        else P.show date_selector_popup)
      date_input
  in
  popup

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
  let layout = L.tower [ menu; month_layout ] in
  let new_event_popup = attach_new_event_popup layout in
  W.on_button_release ~release:(fun _ -> P.toggle new_event_popup) add_event_btn;
  L.on_resize (L.top_house layout) (fun () -> update_display ());
  layout |> of_layout |> run;
  quit ()
