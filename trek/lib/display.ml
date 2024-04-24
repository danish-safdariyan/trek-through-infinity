open Bogue
open Main

let test lst = MonthDisplay.layout_of_month 150 lst |> of_layout |> run
