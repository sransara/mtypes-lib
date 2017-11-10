module type ATOM =
sig
  type t
  val compare: t -> t -> int
end

module type Base = sig
  type t
  type atom

  val empty : t
  val is_empty : t -> bool

  val insert : atom -> t -> t
  val merge : t -> t -> t

  val find_min : t -> atom  (* raises Empty if heap is empty *)
  val delete_min : t -> t   (* raises Empty if heap is empty *)
  val pop_min : t -> atom * t   (* raises Empty if heap is empty *)
end

module type S = sig
  include Base

  (* Patching *)
  type edit =
    | Insert of atom
    | Delete of atom
  
  include Mwrap.PATCHABLE with type t := t and type edit := edit

  (* Merging *)
  include Mwrap.RESOLVEABLE with type t := t
end