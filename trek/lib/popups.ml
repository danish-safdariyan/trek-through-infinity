open Bogue
module L = Layout

type t = {
  screen : L.t;
  popup : L.t;
  mutable state : bool;
}

let get_layout p = p.popup

let update_popup p =
  L.set_show p.screen p.state;
  L.set_show p.popup p.state

let toggle_popup p =
  p.state <- not p.state;
  update_popup p

let show_popup p =
  p.state <- true;
  update_popup p

let hide_popup p =
  p.state <- false;
  update_popup p

let attach_popup layout popup =
  let out = { screen = Popup.attach layout popup; popup; state = false } in
  let _ = hide_popup out in
  out
