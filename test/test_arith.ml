open Base
open Hardcaml
open! Expect_test_helpers_base
module Unsigned = Hardcaml_fixed_point.Unsigned (Bits)
module Signed = Hardcaml_fixed_point.Signed (Bits)

let test
  (type a)
  (module X : Hardcaml_fixed_point.Fixed_point with type t = a and type bits = Bits.t)
  width_int_a
  width_frac_a
  a
  width_int_b
  width_frac_b
  b
  ~f
  ~op
  =
  let afx = X.of_float width_int_a width_frac_a a in
  let bfx = X.of_float width_int_b width_frac_b b in
  let opfx = op afx bfx in
  Stdio.printf
    "%i.%i %i.%i\n ------ %f op %f = %f\n ------ %s op %s = %s = %f\n"
    width_int_a
    width_frac_a
    width_int_b
    width_frac_b
    a
    b
    (f a b)
    (X.signal afx |> Bits.to_bstr)
    (X.signal bfx |> Bits.to_bstr)
    (X.signal opfx |> Bits.to_bstr)
    (X.to_float opfx)
;;

let%expect_test "addition" =
  test (module Unsigned) 2 2 0.75 1 1 0.5 ~f:Float.( + ) ~op:Unsigned.( +: );
  [%expect
    {|
    2.2 1.1
     ------ 0.750000 op 0.500000 = 1.250000
     ------ 0011 op 01 = 00101 = 1.250000
    |}];
  test (module Unsigned) 8 4 123.125 3 9 6.005 ~f:Float.( + ) ~op:Unsigned.( +: );
  [%expect
    {|
    8.4 3.9
     ------ 123.125000 op 6.005000 = 129.130000
     ------ 011110110010 op 110000000010 = 010000001001000010 = 129.128906
    |}];
  test (module Signed) 2 2 0.75 1 1 0.5 ~f:Float.( + ) ~op:Signed.( +: );
  [%expect
    {|
    2.2 1.1
     ------ 0.750000 op 0.500000 = 1.250000
     ------ 0011 op 01 = 00101 = 1.250000
    |}];
  test (module Signed) 8 4 123.125 3 9 2.005 ~f:Float.( + ) ~op:Signed.( +: );
  [%expect
    {|
    8.4 3.9
     ------ 123.125000 op 2.005000 = 125.130000
     ------ 011110110010 op 010000000010 = 001111101001000010 = 125.128906
    |}]
;;

let%expect_test "subtraction" =
  test (module Unsigned) 2 2 0.75 1 1 0.5 ~f:Float.( - ) ~op:Unsigned.( -: );
  [%expect
    {|
    2.2 1.1
     ------ 0.750000 op 0.500000 = 0.250000
     ------ 0011 op 01 = 00001 = 0.250000
    |}];
  test (module Unsigned) 8 4 123.125 3 9 6.005 ~f:Float.( - ) ~op:Unsigned.( -: );
  [%expect
    {|
    8.4 3.9
     ------ 123.125000 op 6.005000 = 117.120000
     ------ 011110110010 op 110000000010 = 001110101000111110 = 117.121094
    |}];
  test (module Signed) 2 2 0.25 1 1 0.5 ~f:Float.( - ) ~op:Signed.( -: );
  [%expect
    {|
    2.2 1.1
     ------ 0.250000 op 0.500000 = -0.250000
     ------ 0001 op 01 = 11111 = -0.250000
    |}];
  test (module Signed) 8 4 123.125 3 9 2.005 ~f:Float.( - ) ~op:Signed.( -: );
  [%expect
    {|
    8.4 3.9
     ------ 123.125000 op 2.005000 = 121.120000
     ------ 011110110010 op 010000000010 = 001111001000111110 = 121.121094
    |}]
;;

let%expect_test "multiplication" =
  test (module Unsigned) 2 2 0.75 1 1 0.5 ~f:Float.( * ) ~op:Unsigned.( *: );
  [%expect
    {|
    2.2 1.1
     ------ 0.750000 op 0.500000 = 0.375000
     ------ 0011 op 01 = 000011 = 0.375000
    |}];
  test (module Signed) 8 4 123.125 3 9 (-2.005) ~f:Float.( * ) ~op:Signed.( *: );
  [%expect
    {|
    8.4 3.9
     ------ 123.125000 op -2.005000 = -246.865625
     ------ 011110110010 op 101111111110 = 111000010010100010011100 = -246.730957
    |}]
;;
