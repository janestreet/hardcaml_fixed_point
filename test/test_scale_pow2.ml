open! Base
open Hardcaml
open! Expect_test_helpers_base
module Unsigned = Hardcaml_fixed_point.Unsigned (Bits)
module Signed = Hardcaml_fixed_point.Signed (Bits)

let sexp_of_unsigned x = [%sexp ((x, Unsigned.to_float x) : Unsigned.t * float)]

let%expect_test "unsigned multiply/divide by 2" =
  let f = Unsigned.of_float 4 3 3.14 in
  let f_mul_2 = Unsigned.scale_pow2 f 1 in
  let f_div_2 = Unsigned.scale_pow2 f (-1) in
  print_s [%message (f : unsigned) (f_mul_2 : unsigned) (f_div_2 : unsigned)];
  [%expect
    {|
    ((f (
       ((s  0011001)
        (fp 3))
       3.125))
     (f_mul_2 (
       ((s  0011001)
        (fp 2))
       6.25))
     (f_div_2 (
       ((s  0011001)
        (fp 4))
       1.5625)))
    |}]
;;

let%expect_test "unsigned scale up, past min fp" =
  let f = Unsigned.of_float 2 1 0.5 in
  let f_mul_2 = Unsigned.scale_pow2 f 1 in
  let f_mul_4 = Unsigned.scale_pow2 f 2 in
  let f_mul_8 = Unsigned.scale_pow2 f 3 in
  print_s
    [%message
      (f : unsigned) (f_mul_2 : unsigned) (f_mul_4 : unsigned) (f_mul_8 : unsigned)];
  [%expect
    {|
    ((f (
       ((s  001)
        (fp 1))
       0.5))
     (f_mul_2 (
       ((s  001)
        (fp 0))
       1))
     (f_mul_4 (
       ((s  0010)
        (fp 0))
       2))
     (f_mul_8 (
       ((s  00100)
        (fp 0))
       4)))
    |}]
;;

let%expect_test "unsigned scale down, past min ip" =
  let f = Unsigned.of_float 2 1 1.5 in
  let f_mul_2 = Unsigned.scale_pow2 f (-1) in
  let f_mul_4 = Unsigned.scale_pow2 f (-2) in
  let f_mul_8 = Unsigned.scale_pow2 f (-3) in
  print_s
    [%message
      (f : unsigned) (f_mul_2 : unsigned) (f_mul_4 : unsigned) (f_mul_8 : unsigned)];
  [%expect
    {|
    ((f (
       ((s  011)
        (fp 1))
       1.5))
     (f_mul_2 (
       ((s  011)
        (fp 2))
       0.75))
     (f_mul_4 (
       ((s  0011)
        (fp 3))
       0.375))
     (f_mul_8 (
       ((s  00011)
        (fp 4))
       0.1875)))
    |}]
;;

let sexp_of_signed x = [%sexp ((x, Signed.to_float x) : Signed.t * float)]

let%expect_test "signed scale up, past min fp" =
  let f = Signed.of_float 2 1 (-0.5) in
  let f_mul_2 = Signed.scale_pow2 f 1 in
  let f_mul_4 = Signed.scale_pow2 f 2 in
  let f_mul_8 = Signed.scale_pow2 f 3 in
  print_s [%message (f : signed) (f_mul_2 : signed) (f_mul_4 : signed) (f_mul_8 : signed)];
  [%expect
    {|
    ((f (
       ((s  111)
        (fp 1))
       -0.5))
     (f_mul_2 (
       ((s  111)
        (fp 0))
       -1))
     (f_mul_4 (
       ((s  1110)
        (fp 0))
       -2))
     (f_mul_8 (
       ((s  11100)
        (fp 0))
       -4)))
    |}]
;;

let%expect_test "signed scale down, past min ip" =
  let f = Signed.of_float 2 1 (-1.5) in
  let f_mul_2 = Signed.scale_pow2 f (-1) in
  let f_mul_4 = Signed.scale_pow2 f (-2) in
  let f_mul_8 = Signed.scale_pow2 f (-3) in
  print_s [%message (f : signed) (f_mul_2 : signed) (f_mul_4 : signed) (f_mul_8 : signed)];
  [%expect
    {|
    ((f (
       ((s  101)
        (fp 1))
       -1.5))
     (f_mul_2 (
       ((s  101)
        (fp 2))
       -0.75))
     (f_mul_4 (
       ((s  1101)
        (fp 3))
       -0.375))
     (f_mul_8 (
       ((s  11101)
        (fp 4))
       -0.1875)))
    |}]
;;
