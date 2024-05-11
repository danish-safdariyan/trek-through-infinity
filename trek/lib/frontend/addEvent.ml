open Bogue
open Backend
open GenDisplay
module L = Layout
module W = Widget
module P = Popups

(** Attaches the popup for adding new events to [layout]; all code for the popup
    of making new events should be put here. *)
let add_event_layout add_event =
  let title_input = W.text_input ~prompt:"Title                 " () in
  let desc_input = W.text_input ~prompt:"Description           " () in
  let repeats = ref Event.Daily in
  let rep_input =
    Select.create
      ~action:(fun input ->
        (repeats :=
           match input with
           | 0 -> Daily
           | 1 -> Weekly
           | 2 -> Monthly
           | 3 -> Yearly
           | _ -> failwith ("Should not reach " ^ string_of_int input));
        print_endline (string_of_int input))
      [| "Once"; "Weekly"; "Monthly"; "Yearly" |]
      0
  in
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
  let out = L.superpose [ surrounding_box stuff; stuff ] in
  let selector = DateSelector.make_selector () in
  let date_selector_popup =
    P.attach_popup out (DateSelector.get_layout selector)
  in
  let on_close () =
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
            ~date:"???" ~repeats:!repeats
        in
        add_event (DateSelector.get_date selector) event;
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
  out
