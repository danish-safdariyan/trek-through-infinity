open Bogue
open Backend
module L = Layout
module W = Widget
module P = Popups

(** Returns the layout of a task *)
let layout_of_task w task =
  let label = L.resident ~x:5 ~y:5 (Task.get_title task |> W.label) in
  let background = Style.color_bg (Draw.opaque (Draw.find_color "#99e3e4")) in
  let line =
    Style.mk_line ~color:(Draw.opaque (Draw.find_color "#008284")) ~width:2 ()
  in
  let border = Style.mk_border ~radius:10 line in
  let box = W.box ~w ~h:30 ~style:(Style.create ~background ~border ()) () in
  let out = L.superpose [ L.resident box; label ] in
  out

(** Popup for adding tasks *)
let addTaskPopup layout update_task_list =
  let title_input = W.text_input ~prompt:"Task Title (Input Here)" () in
  let selector = DateSelector.make_selector () in
  let date_input = W.button (Date.current_date () |> Date.format_date) in
  let add_btn = W.button "Add Task" in
  let cancel_btn = W.button "Cancel" in
  let buttons = L.flat_of_w [ add_btn; cancel_btn ] in
  let content =
    L.tower
      [
        W.label ~size:18 "New Task:" |> L.resident;
        title_input |> L.resident;
        date_input |> L.resident;
        buttons;
      ]
  in
  let popup = L.superpose [ GenDisplay.surrounding_box content; content ] in
  let out = P.attach_popup layout popup in
  let date_selector_popup =
    P.attach_popup layout (DateSelector.get_layout selector)
  in
  let on_close () =
    P.hide date_selector_popup;
    P.hide out;
    W.set_text title_input ""
  in
  let _ =
    W.on_button_release ~release:(fun _ -> on_close ()) cancel_btn;
    W.on_button_release
      ~release:(fun _ ->
        let task_title = W.get_text_input title_input |> Text_input.text in
        let date = W.get_text_input date_input |> Text_input.text in
        (try
           if task_title <> "" && date <> "" then
             update_task_list task_title date
         with _ -> ());
        (*CIRCLE BACK TO DATE ERROR HANDLING LATER*)
        on_close ())
      add_btn;
    P.should_exit_on_press out true;
    P.should_exit_on_press date_selector_popup true;
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

let task_list_layout w h tList update_task_list =
  let today = Date.current_date () in
  let label = W.label ~size:18 "Tasks" |> L.resident in
  let task_list =
    List.map
      (fun task -> layout_of_task (w - 20) task)
      (TaskList.get_tasks today tList)
  in
  let add_tsk_btn = W.button "Add" in
  let header = L.flat [ label; add_tsk_btn |> L.resident ] in
  let () = Space.full_width header in
  let room_list = header :: task_list in
  let taskLayout =
    L.superpose [ GenDisplay.theme_box w h; L.tower room_list ]
  in
  let add_task = addTaskPopup taskLayout update_task_list in
  let _ = W.on_button_release ~release:(fun _ -> P.show add_task) add_tsk_btn in
  taskLayout

let left_side_layout w h new_event task_lst update_task_lst =
  let task_layout = task_list_layout w (h - 305) task_lst update_task_lst in
  let layout = L.tower [ new_event; task_layout ] in
  let box = GenDisplay.theme_box (w + 20) h in
  L.superpose ~scale_content:false [ box; layout ]
