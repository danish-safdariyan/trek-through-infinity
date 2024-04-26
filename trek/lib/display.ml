open Bogue
open Main

let test cal =
  let today = Date.current_date () in
  let month = MonthDisplay.get_month today.month today.year in
  let width = 1000 in
  Sync.push (fun _ ->
      Window.set_size ~w:width ~h:1000
        (Window.create (MonthDisplay.layout_of_month (width / 10) cal month)));
  let lOut = MonthDisplay.layout_of_month (width / 10) cal month in
  Layout.tower [ lOut; Layout.resident (ButtonDisplay.button (width / 10)) ]
  |> of_layout |> run
