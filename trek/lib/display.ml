open Bogue
open Main

let test lst =
  let width = 1000 in
  Sync.push (fun _ ->
      Window.set_size ~w:width ~h:1000
        (Window.create (MonthDisplay.layout_of_month (width / 10) lst)));
  let lOut = MonthDisplay.layout_of_month (width / 10) lst in
  Layout.tower [ lOut; Layout.resident (ButtonDisplay.button (width / 10)) ]
  |> of_layout |> run
