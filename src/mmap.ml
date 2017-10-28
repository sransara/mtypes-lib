module type Base = sig
  type t
  type key
  type atom
  
  val empty: t
  val is_empty: t -> bool
  val add: key -> atom -> t -> t
  val find: key -> t -> atom
  val remove: key -> t -> t
  val mem: key -> t -> bool
  val iter: (key -> atom -> unit) -> t -> unit
  val map: (atom -> atom) -> t -> t
  val mapi: (key -> atom -> atom) -> t -> t
  val fold: (key -> atom -> atom -> atom) -> t -> atom -> atom
  val compare: (atom -> atom -> int) -> t -> t -> int
  val equal: (atom -> atom -> bool) -> t -> t -> bool
end