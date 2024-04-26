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
