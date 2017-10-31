module type KEY =
sig
  type t
  val compare: t -> t -> int
end

module type ATOM = sig
  type t
  val equal: t -> t -> bool
  include Mtypes.RESOLVEABLE with type t := t
end

module type Base = sig
  type t
  type key
  type atom

  val empty : unit -> t
  val is_empty : t -> bool
  val mem : key -> t -> bool
  val add : key -> atom -> t -> t
  val remove : key -> t -> t
  val compare : (atom -> atom -> int) -> t -> t -> int
  val equal : t -> t -> bool
  val iter : (key -> atom -> unit) -> t -> unit
  val fold : (key -> atom -> atom -> atom) -> t -> atom -> atom
  val find : key -> t -> atom
  val map : (atom -> atom) -> t -> t
  val mapi : (key -> atom -> atom) -> t -> t
end

module type S = sig
  include Base

  (* Patching *)
  type edit = 
    | Add of key * atom
    | Remove of key
    | Replace of key * atom * atom

  include Mtypes.PATCHABLE with type t := t and type edit := edit

  (* Merging *)
  include Mtypes.RESOLVEABLE with type t := t
end