module type Map = sig
  type ('k, 'v) t
  (** [('k, 'v) t] is the type of maps that bind keys of type ['k] to values of
      type ['v]. *)

  val empty : ('k, 'v) t
  (** [empty] does not bind any keys. *)

  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  (** [insert k v m] is the map that binds [k] to [v], and also contains all the
      bindings of [m]. If [k] was already bound in [m], that old binding is
      superseded by the binding to [v] in the returned map. *)

  val lookup : 'k -> ('k, 'v) t -> 'v
  (** [lookup k m] is the value bound to [k] in [m]. Raises: [Not_found] if [k]
      is not bound in [m]. *)

  val keys : ('k, 'v) t -> 'k list
  (** [keys m] is an association list containing the same keys as [m]. The keys
      in the list are guaranteed to be unique. *)

  val bindings : ('k, 'v) t -> ('k * 'v) list
  (** [bindings m] is an association list containing the same bindings as [m].
      The keys in the list are guaranteed to be unique. *)

  val to_list : ('k, 'v) t -> ('k * 'v) list
  (**[to_list] takes in a map and returns a k v list*)

  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
  (**[remove] takes in a k and k v map. It searches for k removes the k v pair
     if found *)
end

(* type event = Day of int * int * int *)

module AssocListMap : Map = struct
  type ('k, 'v) t = ('k * 'v) list
  (** The list [(k1, v1); ...; (kn, vn)] binds key [ki] to value [vi]. If a key
      appears more than once in the list, it is bound to the the left-most
      occurrence in the list. *)

  let empty = []
  let insert k v m = (k, v) :: m
  let lookup k m = List.assoc k m
  let keys m = List.(m |> map fst |> sort_uniq Stdlib.compare)
  let bindings m = m |> keys |> List.map (fun k -> (k, lookup k m))
  let to_list m : ('a * 'b) list = m

  (* let rec compare_lists list1 list2 compare_ele = match (list1, list2) with |
     [], [] -> 0 | [], _ -> 1 | _, [] -> 1 | (k1,v1) :: rest1, (k2,v2) :: rest2
     -> let ele_comparison = compare_ele v1 v2 in if ele_comparison = 0 then
     compare_lists rest1 rest2 compare_ele else ele_comparison *)
  let rec remove k m =
    match m with
    | [] -> m
    | (k1, v1) :: t -> if k1 = k then t else (k1, v1) :: remove k t
end
