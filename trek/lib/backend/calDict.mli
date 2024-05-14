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

  val keys :('k, 'v) t -> 'k list
  (** [keys m] is an association list containing the same keys as [m].
      The keys in the list are guaranteed to be unique. *)
  val bindings : ('k, 'v) t -> ('k * 'v) list
  (** [bindings m] is an association list containing the same bindings as [m].
      The keys in the list are guaranteed to be unique. *)
  val to_list : ('k, 'v) t -> ('k * 'v) list
  (**[to_list] takes in a map and returns a k v list*)

  (* val compare_lists : ('k, 'v) t -> ('k, 'v) t -> ('v -> 'v -> int) -> int
  (**[compare_lists] compares two lists [list1] and [list2] and applies the 
      function [compare_ele] in order to provide comparisons for different types*) *)

  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
  (**[remove] takes in a k and k v map. It searches for k removes the k v pair if found *)
end

module AssocListMap : Map



