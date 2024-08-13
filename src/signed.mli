open Hardcaml

module Make (Bits : Comb.S) :
  Fixed_intf.Fixed_point
  with type bits = Bits.t
   and type with_zero_width = Bits.With_zero_width.t
