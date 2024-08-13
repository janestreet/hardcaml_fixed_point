open Hardcaml

module type Fixed_point = Fixed_intf.Fixed_point

module Unsigned (Bits : Comb.S) :
  Fixed_point with type bits = Bits.t and type with_zero_width = Bits.With_zero_width.t =
  Unsigned.Make (Bits)

module Signed (Bits : Comb.S) :
  Fixed_point with type bits = Bits.t and type with_zero_width = Bits.With_zero_width.t =
  Signed.Make (Bits)
