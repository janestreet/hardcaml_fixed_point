open Hardcaml

module type Fixed = Fixed_intf.Fixed_point

module Unsigned (Bits : Comb.S) = Unsigned.Make (Bits)
module Signed (Bits : Comb.S) = Signed.Make (Bits)
