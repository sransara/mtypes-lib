module type ATOM =
sig
  type t
  val compare: t -> t -> int
end

module type Base = sig
  type t
  type atom

  val empty: t
  val is_empty: t -> bool
  val mem: atom -> t -> bool
  val add: atom -> t -> t
  val singleton: atom -> t
  val remove: atom -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: (atom -> unit) -> t -> unit
  val fold: (atom -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (atom -> bool) -> t -> bool
  val exists: (atom -> bool) -> t -> bool
  val filter: (atom -> bool) -> t -> t
  val partition: (atom -> bool) -> t -> t * t
  val cardinal: t -> int
  val elements: t -> atom list
  val min_elt: t -> atom
  val max_elt: t -> atom
  val choose: t -> atom
  val split: atom -> t -> t * bool * t
  val find: atom -> t -> atom
  val of_list: atom list -> t
end

module type S = sig
  include Base
  (* Patching *)
  type edit = 
    | Add of atom
    | Remove of atom

  include Mwrap.PATCHABLE with type t := t and type edit := edit

  (* Merging *)
  include Mwrap.MERGEABLE with type t := t
end