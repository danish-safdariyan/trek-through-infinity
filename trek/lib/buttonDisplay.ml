open Bogue
module W = Widget
module L = Layout

let button w =
  W.button ~kind:Trigger ~border_radius:w
    ~border_color:(Draw.opaque Draw.dark_grey)
    ~fg:(Draw.opaque Draw.pale_grey)
    ~bg_on:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~bg_off:(Style.color_bg (Draw.opaque Draw.pale_grey))
    ~bg_over:(Some (Style.color_bg (Draw.opaque Draw.pale_grey)))
    ~label:(W.get_label (W.label "+"))
    ~label_on:(W.get_label (W.label "+"))
    ~label_off:(W.get_label (W.label "+"))
    ~state:false
    ~action:(fun _ -> print_endline "Mike Wazowski")
    "Hello"

let prev_btn get_month update_month =
  W.button ~kind:Button.Trigger
    ~label:(Label.create ~align:Draw.Center "<")
    ~action:(fun _ ->
      let m, y = get_month () in
      update_month (Date.prev_month m y))
    "Prev month"

let next_btn get_month update_month =
  W.button ~kind:Button.Trigger
    ~label:(Label.create ~align:Draw.Center ">")
    ~action:(fun _ ->
      let m, y = get_month () in
      update_month (Date.next_month m y))
    "Next month"
