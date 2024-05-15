open Backend
open Bogue
open GenDisplay
module L = Layout
module W = Widget
module P = Popups

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

let edit_event_popup date event layout update_calendar on_update =
  let title_input = W.text_input ~prompt:"Title" () in
  let desc_input = W.text_input ~prompt:"Description" () in
  let update_btn = W.button "OK" in
  let cancel_btn = W.button "Cancel" in
  let buttons = L.flat_of_w [ update_btn; cancel_btn ] in
  let room_list =
    [
      W.label ~size:18 "New Event:" |> L.resident;
      title_input |> L.resident;
      desc_input |> L.resident;
      buttons;
    ]
  in
  let () =
    List.iter
      (fun ch -> Space.full_width ~right_margin:10 ~left_margin:10 ch)
      room_list
  in
  let stuff = L.tower room_list in
  let popup = L.superpose [ surrounding_box stuff; stuff ] in
  let out = P.attach_popup layout popup in
  let on_close () =
    P.hide out;
    W.set_text title_input (Event.get_title event);
    W.set_text desc_input (Event.get_description event)
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
                ~description:(W.get_text_input desc_input |> Text_input.text)));
        on_update ();
        on_close ())
      update_btn;
    P.should_exit_on_press out true
  in
  out

let task_info_popup date event layout update_calendar =
  let title =
    W.label ~size:20 ~align:Draw.Center (Event.get_title event) |> L.resident
  in
  let description = W.text_display (Event.get_description event) in
  let edit_btn = W.button "Edit" in
  let delete_btn = W.button "Delete" in
  let exit_btn = W.button "Exit" in
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

let layout_of_event w date event layout update_calendar =
  let text = Event.get_title event in
  let label = W.label text in
  let _ =
    let temp = W.get_label label in
    if fst (Label.size temp) > w - 13 then (
      Label.set temp (text ^ "...");
      while
        fst (Label.size temp) > w - 13 && String.length (W.get_text label) > 1
      do
        let text = W.get_text label in
        if String.length text < 4 then Label.set temp " "
        else Label.set temp (String.sub text 0 (String.length text - 4) ^ "...")
      done)
  in
  let label_l = L.resident ~x:5 ~y:5 label in
  let background = Style.color_bg (Draw.opaque Draw.pale_grey) in
  let line = Style.mk_line ~color:(Draw.opaque Draw.dark_grey) ~width:2 () in
  let border = Style.mk_border ~radius:10 line in
  let box = W.box ~w ~h:30 ~style:(Style.create ~background ~border ()) () in
  let out = L.superpose ~name:"Task" ~w ~h:30 [ L.resident box; label_l ] in
  let popup = task_info_popup date event layout update_calendar in
  let _ =
    W.on_release ~release:(fun _ -> P.show popup) label;
    W.on_release ~release:(fun _ -> P.show popup) box
  in
  out
