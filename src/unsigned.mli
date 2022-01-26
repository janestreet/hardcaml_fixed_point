open Hardcaml
module Make (Bits : Comb.S) : Fixed_intf.Fixed_point with type bits = Bits.t
