open Bogue
open Backend
open GenDisplay
module L = Layout
module W = Widget
module P = Popups

(** Popup for deleting tasks *)
let delete_task_popup task layout update_task_list on_update =
  let yes = W.button "Yes" in
  let no = W.button "No" in
  let text =
    W.text_display ~w:200 "Are you sure? This action cannot be undone."
  in
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
        update_task_list (TaskList.remove_task task))
      yes;
    P.should_exit_on_press out true
  in
  out

(** Popup for editing tasks *)
let edit_task_popup task layout update_task_list on_update =
  let title_input = W.text_input ~prompt:"Task Title (Input Here)" () in
  let selector = DateSelector.make_selector () in
  let date_input = W.button (Date.current_date () |> Date.format_date) in
  let confirm_btn = W.button "Confirm" in
  let cancel_btn = W.button "Cancel" in
  let buttons = L.flat_of_w [ confirm_btn; cancel_btn ] in
  let content =
    L.tower
      [
        W.label ~size:18 "Edit Task:" |> L.resident;
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
    W.set_text title_input "";
    on_update ();
    DateSelector.set_date selector (Task.get_date task)
  in
  let _ =
    DateSelector.set_date selector (Task.get_date task);
    W.set_text title_input (Task.get_title task);
    W.on_button_release ~release:(fun _ -> on_close ()) cancel_btn;
    W.on_button_release
      ~release:(fun _ ->
        let task_title = W.get_text_input title_input |> Text_input.text in
        let date = DateSelector.get_date selector in
        if task_title <> "" then
          update_task_list
            (Task.create ~title:task_title ~date |> TaskList.replace_task task);
        on_close ())
      confirm_btn;
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

(** Popup with details about task *)
let task_info_popup task layout update_task_list =
  let title =
    W.label ~size:15 ~align:Draw.Center (Task.get_title task) |> L.resident
  in
  let description =
    W.label ~size:15 ~align:Draw.Center
      ("Due: " ^ (Task.get_date task |> Date.format_date))
    |> L.resident
  in
  let edit_btn = W.button ~border_radius:10 "Edit" in
  let delete_btn = W.button ~border_radius:10 "Delete" in
  let exit_btn = W.button ~border_radius:10 "X" in
  let buttons =
    L.flat
      [
        L.resident edit_btn;
        L.resident delete_btn;
        Space.hfill ();
        L.resident exit_btn;
      ]
  in
  let info = L.tower [ title; description; buttons ] in
  let box = surrounding_box info in
  let popup = L.superpose [ box; info ] in
  let out = P.attach_popup layout popup in
  let delete_task =
    delete_task_popup task layout update_task_list (fun () -> P.hide out)
  in
  let edit_task =
    edit_task_popup task layout update_task_list (fun () -> P.hide out)
  in
  let _ =
    P.should_exit_on_press out true;
    W.on_button_release ~release:(fun _ -> P.show delete_task) delete_btn;
    W.on_button_release ~release:(fun _ -> P.show edit_task) edit_btn;
    W.on_button_release ~release:(fun _ -> P.hide out) exit_btn;
    Space.full_width title;
    Space.full_width description;
    Space.full_width buttons
  in
  out

(** Returns the layout of a task *)
let layout_of_task w task layout update_task_list =
  let label =
    Task.get_title task |> W.label ~fg:(Draw.find_color "#003232" |> Draw.opaque)
  in
  let background = Style.color_bg (Draw.opaque (Draw.find_color "#99e3e4")) in
  let line =
    Style.mk_line ~color:(Draw.opaque (Draw.find_color "#008284")) ~width:2 ()
  in
  let border = Style.mk_border ~radius:10 line in
  let box = W.box ~w ~h:30 ~style:(Style.create ~background ~border ()) () in
  let out = L.superpose [ L.resident box; L.resident ~x:5 ~y:5 label ] in
  let popup = task_info_popup task layout update_task_list in
  let _ =
    adjust_label_size (w - 13) label;
    W.on_release ~release:(fun _ -> P.show popup) box;
    W.on_release ~release:(fun _ -> P.show popup) label
  in
  out

(** Popup for adding tasks *)
let add_task_popup layout update_task_list =
  let title_input = W.text_input ~prompt:"Task Title (Input Here)" () in
  let selector = DateSelector.make_selector () in
  let date_input =
    W.button ~border_radius:10 (Date.current_date () |> Date.format_date)
  in
  let add_btn = W.button ~border_radius:10 "Add Task" in
  let cancel_btn = W.button ~border_radius:10 "Cancel" in
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
    W.set_text title_input "";
    DateSelector.set_date selector (Date.current_date ())
  in
  let _ =
    W.on_button_release ~release:(fun _ -> on_close ()) cancel_btn;
    W.on_button_release
      ~release:(fun _ ->
        let task_title = W.get_text_input title_input |> Text_input.text in
        let date = DateSelector.get_date selector in
        if task_title <> "" then
          update_task_list
            (Task.create ~title:task_title ~date |> TaskList.add_task);
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

(** Layout for tasks for given date *)
let task_layout_of_day w date t_list background update_task_list =
  let tasks = TaskList.get_tasks date t_list in
  match tasks with
  | [] -> L.empty ~w:0 ~h:0 ()
  | lst ->
      let header = W.label (Date.format_date date ^ ":") |> L.resident in
      let task_list =
        List.map
          (fun task -> layout_of_task (w - 20) task background update_task_list)
          lst
      in
      L.tower ~hmargin:0 (header :: task_list)

(** Layout for task list *)
let task_list_layout w h t_list update_task_list =
  let background = L.empty ~w ~h () in
  let today = Date.current_date () in
  let label = W.label ~size:18 "Tasks" |> L.resident in
  let rec helper day count =
    if count > 0 then
      task_layout_of_day (w - 20) day t_list background update_task_list
      :: helper (Date.next_day day) (count - 1)
    else []
  in
  let add_tsk_btn = W.button ~border_radius:10 "Add" in
  let header = L.flat [ label; Space.hfill (); add_tsk_btn |> L.resident ] in
  let tasks =
    L.superpose
      [ L.empty ~w:(w - 20) ~h:(h - 100) (); helper today 10 |> L.tower ]
  in
  let tasks = L.make_clip ~scrollbar:false ~h:(h - 100) tasks in
  let _ = Space.full_width header in
  let room_list =
    [ L.superpose [ L.empty ~w:(w - 20) ~h:30 (); header ]; tasks ]
  in
  let taskLayout =
    L.superpose [ background; GenDisplay.theme_box w h; L.tower room_list ]
  in
  let add_task = add_task_popup taskLayout update_task_list in
  let _ = W.on_button_release ~release:(fun _ -> P.show add_task) add_tsk_btn in
  taskLayout

let left_side_layout w h new_event task_lst update_task_lst =
  let task_layout = task_list_layout w (h - 305) task_lst update_task_lst in
  let layout = L.tower [ new_event; task_layout ] in
  let box = GenDisplay.theme_box (w + 20) h in
  L.superpose ~scale_content:false [ box; layout ]
