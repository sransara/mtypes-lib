module type PATCHABLE = sig
  type t
  type edit
  type patch = edit list
  val op_diff: t -> t -> patch
  val op_transform: patch -> patch -> patch * patch 
end

module type MERGEABLE = sig
  type t
  val resolve: t -> t -> t
  val merge3: ancestor:t -> t -> t -> t
end