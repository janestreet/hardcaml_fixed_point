open! Base
open Hardcaml

module Make (B : Comb.S) : sig
  type bits = B.t

  type t =
    { s : B.t
    ; fp : int
    }
  [@@deriving sexp_of]

  val get_int : int -> bits -> bits
  val get_frac : int -> bits -> bits
  val floor : int -> bits -> bits
  val ceil : int -> bits -> bits
  val half : int -> bits -> bits
  val create : int -> bits -> t
  val int : t -> bits
  val frac : t -> bits
  val signal : t -> bits
  val width_int : t -> int
  val width_frac : t -> int
  val map : t -> f:(bits -> bits) -> t
  val raise_extend : int -> 'a
  val raise_select_int : int -> 'a
  val raise_select_frac : int -> 'a
  val scale_pow2 : ex:(bits -> bits) -> t -> int -> t
end
