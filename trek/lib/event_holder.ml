module type EventHolder = sig
  type date
  type repeat_option
  type event
  type t

  val empty : t
  val find : date -> t -> event array option
  val custom_map : ('a -> 'b) -> 'a array -> 'b array
  val custom_append : 'a array -> 'a array -> 'a array
  val insert : date -> event -> t -> t
  val filter : ('a -> bool) -> 'a array -> 'a array
  val remove : date -> t -> t
end

module EventHolder = struct
  type date = Date.t
  type repeat_option = Event.repeat_option
  type event = Event.t
  type t = (date * event array) array

  let empty : t = [||]

  let find date events_holder =
    let pred (d, _) = d = date in
    let rec loop i =
      if i >= Array.length events_holder then None
      else if pred events_holder.(i) then Some (snd events_holder.(i))
      else loop (i + 1)
    in
    loop 0

  let custom_map f arr =
    let rec map_rec f arr i =
      if i >= Array.length arr then [] else f arr.(i) :: map_rec f arr (i + 1)
    in
    Array.of_list (map_rec f arr 0)

  let custom_append arr1 arr2 =
    let len1 = Array.length arr1 in
    let len2 = Array.length arr2 in
    let result = Array.make (len1 + len2) arr1.(0) in
    Array.blit arr1 0 result 0 len1;
    Array.blit arr2 0 result len1 len2;
    result

  let insert date event events =
    let pred (d, _) = d = date in
    match find date events with
    | Some event_array ->
        if Array.length event_array >= 10 then
          failwith "Exceeded maximum number of events per date"
        else
          let new_event_array = custom_append event_array [| event |] in
          custom_map
            (fun (d', events') ->
              if d' = date then (d', new_event_array) else (d', events'))
            events
    | None ->
        let new_entry = (date, [| event |]) in
        if Array.length events = 0 then [| new_entry |]
        else custom_append events [| new_entry |]

  let filter_array pred arr =
    let rec filter_rec pred arr acc i =
      if i >= Array.length arr then Array.of_list (List.rev acc)
      else if pred arr.(i) then filter_rec pred arr (arr.(i) :: acc) (i + 1)
      else filter_rec pred arr acc (i + 1)
    in
    filter_rec pred arr [] 0

  let remove date events =
    let filtered_events = filter_array (fun (d, _) -> d <> date) events in
    filtered_events
end
