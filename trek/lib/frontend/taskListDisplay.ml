open Bogue
module L = Layout
module W = Widget
module P = Popups

(** Returns the layout of a task *)
let layout_of_task w task =
  let label = L.resident ~x:5 ~y:5 (W.label task) in
  let background = Style.color_bg (Draw.opaque (Draw.find_color "#99e3e4")) in
  let line =
    Style.mk_line ~color:(Draw.opaque (Draw.find_color "#008284")) ~width:2 ()
  in
  let border = Style.mk_border ~radius:10 line in
  let box = W.box ~w:80 ~h:30 ~style:(Style.create ~background ~border ()) () in
  let out = L.superpose ~w:80 ~h:30 [ L.resident box; label ] in
  out

(** Popup for adding tasks *)
let addTaskPopup layout update_task_list =
  let title_input = W.text_input ~prompt:"Task Title" () in
  let add_btn = W.button "Add Task" in
  let cancel_btn = W.button "Cancel" in
  let buttons = L.flat_of_w [ add_btn; cancel_btn ] in
  let content =
    L.tower
      [
        W.label ~size:18 "New Task:" |> L.resident;
        title_input |> L.resident;
        buttons;
      ]
  in
  let popup = L.superpose [ GenDisplay.surrounding_box content; content ] in
  let out = P.attach_popup layout popup in
  let on_close () =
    P.hide out;
    W.set_text title_input ""
  in
  let _ =
    W.on_button_release ~release:(fun _ -> on_close ()) cancel_btn;
    W.on_button_release
      ~release:(fun _ ->
        let task_title = W.get_text_input title_input |> Text_input.text in
        if task_title <> "" then update_task_list task_title;
        on_close ())
      add_btn;
    P.should_exit_on_press out true
  in
  out

let task_list_layout w h tList update_task_list =
  let label = W.label ~size:18 "Tasks" |> L.resident in
  let task_list = List.map (fun task -> layout_of_task w task) tList in
  let add_tsk_btn = W.button "+" in
  let header = L.flat [ label; add_tsk_btn |> L.resident ] in
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
