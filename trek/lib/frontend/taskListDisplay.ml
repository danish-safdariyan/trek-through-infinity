open Backend
open Bogue
module L = Layout
module W = Widget
module P = Popups

let layout_of_task task =
  let label = L.resident ~x:5 ~y:5 task in
  let background = Style.color_bg (Draw.opaque (Draw.find_color "#99e3e4")) in
  let line =
    Style.mk_line ~color:(Draw.opaque (Draw.find_color "#008284")) ~width:2 ()
  in
  let border = Style.mk_border ~radius:10 line in
  let box = W.box ~w:80 ~h:30 ~style:(Style.create ~background ~border ()) () in
  let out = L.superpose ~w:80 ~h:30 [ L.resident box; label ] in
  out

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

let taskListLayout tList add_task_popup =
  let label = W.label ~size:18 "Tasks" |> L.resident in
  let task_list = List.map (fun task -> layout_of_task (W.label task)) tList in
  let add_tsk_btn = W.button "Add" in
  let header =
    L.flat [ label; add_tsk_btn |> L.resident; W.button "Clear" |> L.resident ]
  in
  let room_list = header :: task_list in
  let taskLayout =
    L.superpose
      [
        GenDisplay.surrounding_box ~width:3 (L.tower room_list);
        L.tower room_list;
      ]
  in
  let () =
    List.iter
      (fun ch -> Space.full_width ~right_margin:10 ~left_margin:10 ch)
      room_list
  in
  let () =
    W.on_button_release
      ~release:(fun _ -> P.show (addTaskPopup taskLayout add_task_popup))
      add_tsk_btn
  in
  let () = Space.full_width taskLayout in
  taskLayout
