(* module type EventHolder = sig type date type repeat_option type event type t

   val empty : t val find : date -> t -> event array option val custom_map : ('a
   -> 'b) -> 'a array -> 'b array val custom_append : 'a array -> 'a array -> 'a
   array val insert : date -> event -> t -> t val filter : ('a -> bool) -> 'a
   array -> 'a array val remove : date -> t -> t end *)
