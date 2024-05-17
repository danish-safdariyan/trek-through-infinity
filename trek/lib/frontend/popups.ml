open Bogue
module W = Widget
module L = Layout

type t = {
  screen : L.t;
  popup : L.t;
  mutable state : bool;
}

let get_layout p = p.popup

(** Updates the show state of [p] to match [p.state]. *)
let update p =
  L.set_show p.screen p.state;
  L.set_show p.popup p.state

let toggle p =
  p.state <- not p.state;
  update p

let show p =
  p.state <- true;
  update p

let hide p =
  p.state <- false;
  update p

let get_state p = p.state

let attach_popup ?bg layout popup =
  let out =
    {
      screen =
        Popup.attach
          ~bg:
            (match bg with
            | None -> Draw.none
            | Some bg -> bg)
          layout popup;
      popup;
      state = false;
    }
  in
  let _ = hide out in
  out

(** Applies a function to all widgets in a layout. *)
let rec apply f = function
  | L.Rooms lst -> List.iter (fun c -> L.get_content c |> apply f) lst
  | Resident w -> f w

let should_exit_on_press popup b =
  if b then
    apply (W.on_click ~click:(fun _ -> hide popup)) (L.get_content popup.screen)
  else apply (W.on_click ~click:(fun _ -> ())) (L.get_content popup.screen)
