open Backend
open Bogue
open GenDisplay
module L = Layout
module W = Widget
module P = Popups

(** Popup for deleting events *)
let delete_event_popup date event layout update_calendar on_update =
  match Event.get_repeats event with
  | NoRepeat ->
      let yes = W.button "Yes" in
      let no = W.button "No" in
      let text = W.text_display "Are you sure? This action cannot be undone." in
      let buttons = L.flat_of_w [ yes; no ] in
      let info = L.tower [ L.resident text; buttons ] in
      let box = surrounding_box info in
      let popup = L.superpose [ box; info ] in
      let out = P.attach_popup layout popup in
      let _ =
        W.on_button_release ~release:(fun _ -> P.hide out) no;
        W.on_button_release
          ~release:(fun _ ->
            P.hide out;
            on_update ();
            update_calendar (Calendar.remove_event date event))
          yes;
        P.should_exit_on_press out true
      in
      out
  | _ ->
      let once = W.button "Just once" in
      let all_future = W.button "All future times" in
      let all = W.button "All instances" in
      let cancel = W.button "Cancel" in
      let text =
        W.text_display
          "Do you want to delete just this event, all future instances of this \
           event, or all instances of this event?"
      in
      let info = L.tower_of_w [ text; once; all_future; all; cancel ] in
      let box = surrounding_box info in
      let popup = L.superpose [ box; info ] in
      let out = P.attach_popup layout popup in
      let _ =
        W.on_button_release
          ~release:(fun _ ->
            P.hide out;
            on_update ())
          cancel;
        W.on_button_release
          ~release:(fun _ ->
            P.hide out;
            on_update ();
            update_calendar (Calendar.remove_event date event))
          all;
        W.on_button_release
          ~release:(fun _ ->
            P.hide out;
            on_update ();
            update_calendar
              (Calendar.edit_event date event
                 (Event.add_condition (fun d -> Date.compare d date < 0) event)))
          all_future;
        W.on_button_release
          ~release:(fun _ ->
            P.hide out;
            on_update ();
            update_calendar
              (Calendar.edit_event date event
                 (Event.add_condition (fun d -> Date.compare d date <> 0) event)))
          once;
        P.should_exit_on_press out true
      in
      out

(** Popup for editing events *)
let edit_event_popup date event layout update_calendar on_update =
  let title_input = W.text_input ~prompt:"Title (Input Here)" () in
  let desc_input = W.text_input ~prompt:"Description (Input Here)" () in
  let color_input = ColorSelector.make_selector () in
  let update_btn = W.button "OK" in
  let cancel_btn = W.button "Cancel" in
  let buttons = L.flat_of_w [ update_btn; cancel_btn ] in
  let room_list =
    [
      W.label ~size:18 "Edit Event:" |> L.resident;
      title_input |> L.resident;
      desc_input |> L.resident;
      ColorSelector.get_layout color_input;
      buttons;
    ]
  in
  let stuff = L.tower room_list in
  let _ =
    List.iter
      (fun ch -> Space.full_width ~right_margin:10 ~left_margin:10 ch)
      room_list;
    Space.full_width stuff;
    ColorSelector.set_color (Event.get_color event) color_input
  in
  let popup = L.superpose [ theme_box 300 (L.height stuff); stuff ] in
  let out = P.attach_popup layout popup in
  let on_close () =
    P.hide out;
    W.set_text title_input (Event.get_title event);
    W.set_text desc_input (Event.get_description event);
    ColorSelector.set_color (Event.get_color event) color_input
  in
  let _ =
    W.set_text title_input (Event.get_title event);
    W.set_text desc_input (Event.get_description event);
    W.on_button_release ~release:(fun _ -> on_close ()) cancel_btn;
    W.on_button_release
      ~release:(fun _ ->
        update_calendar
          (Calendar.edit_event date event
             (Event.edit event
                ~title:(W.get_text_input title_input |> Text_input.text)
                ~description:(W.get_text_input desc_input |> Text_input.text)
                ~color:(ColorSelector.get_color color_input)));
        on_update ();
        on_close ())
      update_btn;
    P.should_exit_on_press out true
  in
  out

(** Popup with details about event *)
let event_info_popup date event layout update_calendar =
  let title =
    W.label ~size:20 ~align:Draw.Center (Event.get_title event) |> L.resident
  in
  let description = W.text_display (Event.get_description event) in
  let edit_btn = W.button "Edit" in
  let delete_btn = W.button "Delete" in
  let exit_btn = W.button "X" in
  let buttons =
    L.flat
      [
        L.resident edit_btn;
        L.resident delete_btn;
        Space.hfill ();
        L.resident exit_btn;
      ]
  in
  let info = L.tower [ title; L.resident description; buttons ] in
  let box = surrounding_box info in
  let popup = L.superpose [ box; info ] in
  let out = P.attach_popup layout popup in
  let delete_event =
    delete_event_popup date event layout update_calendar (fun () -> P.hide out)
  in
  let edit_event =
    edit_event_popup date event layout update_calendar (fun () -> P.hide out)
  in
  let _ =
    P.should_exit_on_press out true;
    W.on_button_release ~release:(fun _ -> P.show delete_event) delete_btn;
    W.on_button_release ~release:(fun _ -> P.show edit_event) edit_btn;
    W.on_button_release ~release:(fun _ -> P.hide out) exit_btn;
    Space.full_width title;
    Space.full_width buttons
  in
  out

(** outline, background *)
let get_color = function
  | Event.Red -> (Draw.red, Draw.find_color "#ECAAAA")
  | Orange -> (Draw.find_color "#C57106", Draw.find_color "#ECCB86")
  | Yellow -> (Draw.find_color "#CFAF07", Draw.find_color "#F4F2C4")
  | Green -> (Draw.find_color "#08542A", Draw.find_color "#C4F4C9")
  | Blue -> (Draw.blue, Draw.find_color "#C4E9F4")
  | Indigo -> (Draw.find_color "#00796E", Draw.find_color "#C0f2ee")
  | Violet -> (Draw.find_color "#540853", Draw.find_color "#F4C4EE")

let layout_of_event w date event layout update_calendar =
  let color = Event.get_color event |> get_color in
  let text = Event.get_title event in
  let label = W.label ~fg:(fst color |> Draw.opaque) text in
  let _ = adjust_label_size (w - 13) label in
  let label_l = L.resident ~x:5 ~y:5 label in
  let background = Style.color_bg (snd color |> Draw.opaque) in
  let line = Style.mk_line ~color:(fst color |> Draw.opaque) ~width:2 () in
  let border = Style.mk_border ~radius:10 line in
  let box = W.box ~w ~h:30 ~style:(Style.create ~background ~border ()) () in
  let out = L.superpose ~name:"Event" ~w ~h:30 [ L.resident box; label_l ] in
  let popup = event_info_popup date event layout update_calendar in
  let _ =
    W.on_release ~release:(fun _ -> P.show popup) label;
    W.on_release ~release:(fun _ -> P.show popup) box
  in
  out

let add_event_layout update_calendar =
  let title_input = W.text_input ~prompt:"Title (Input Here)" () in
  let desc_input = W.text_input ~prompt:"Description (Input Here)" () in
  let repeats = ref Event.NoRepeat in
  let rep_input =
    Select.create
      ~action:(fun input ->
        repeats :=
          match input with
          | 0 -> NoRepeat
          | 1 -> Daily
          | 2 -> Weekly
          | 3 -> Monthly
          | 4 -> Yearly
          | _ -> failwith ("Should not reach " ^ string_of_int input))
      [| "Once"; "Daily"; "Weekly"; "Monthly"; "Yearly" |]
      0
  in
  let date_input =
    W.button ~border_radius:10 (Date.current_date () |> Date.format_date)
  in
  let color_input = ColorSelector.make_selector () in
  let create_btn = W.button ~border_radius:10 "OK" in
  let cancel_btn = W.button ~border_radius:10 "Cancel" in
  let buttons = L.flat_of_w [ create_btn; cancel_btn ] in
  let room_list =
    [
      W.label ~size:18 "New Event:" |> L.resident;
      title_input |> L.resident;
      date_input |> L.resident;
      desc_input |> L.resident;
      rep_input;
      ColorSelector.get_layout color_input;
      buttons;
    ]
  in
  let () =
    List.iter
      (fun ch -> Space.full_width ~right_margin:10 ~left_margin:10 ch)
      room_list
  in
  let stuff = L.tower room_list in
  let out =
    L.superpose ~scale_content:false [ surrounding_box stuff ~width:3; stuff ]
  in
  let selector = DateSelector.make_selector () in
  let date_selector_popup =
    P.attach_popup out (DateSelector.get_layout selector)
  in
  let on_close () =
    P.hide date_selector_popup;
    W.set_text title_input "";
    W.set_text desc_input "";
    DateSelector.set_date selector (Date.current_date ());
    ColorSelector.set_color Event.Red color_input
  in
  let _ =
    P.should_exit_on_press date_selector_popup true;
    W.on_button_release ~release:(fun _ -> on_close ()) cancel_btn;
    W.on_button_release
      ~release:(fun _ ->
        update_calendar
          (Calendar.add_event
             (DateSelector.get_date selector)
             (W.get_text_input title_input |> Text_input.text)
             (W.get_text_input desc_input |> Text_input.text)
             !repeats
             (ColorSelector.get_color color_input));
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
