open! Base
open Hardcaml
open! Expect_test_helpers_base
module Unsigned = Hardcaml_fixed_point.Unsigned (Bits)
module Signed = Hardcaml_fixed_point.Signed (Bits)

let test_round
  (type a)
  (module X : Hardcaml_fixed_point.Fixed_point with type t = a and type bits = Bits.t)
  width_int_a
  width_frac_a
  a
  width_int_b
  width_frac_b
  =
  let fx = X.of_float width_int_a width_frac_a a in
  let opfx = X.resize fx width_int_b width_frac_b in
  Stdio.printf
    "%f[%i:%i] | %f %s[%i:%i] = %s = %f\n"
    a
    width_int_a
    width_frac_a
    (X.to_float fx)
    (fx |> X.signal |> Bits.to_bstr)
    width_int_b
    width_frac_b
    (opfx |> X.signal |> Bits.to_bstr)
    (X.to_float opfx)
;;

let%expect_test "simple rounding" =
  test_round (module Unsigned) 3 6 3.12 3 4;
  [%expect {| 3.120000[3:6] | 3.109375 011000111[3:4] = 0110001 = 3.062500 |}]
;;

let unsigned_rounding_ops =
  [ Unsigned.Round.neg_infinity
  ; Unsigned.Round.pos_infinity
  ; Unsigned.Round.to_zero
  ; Unsigned.Round.away_from_zero
  ; Unsigned.Round.tie_to_neg_infinity
  ; Unsigned.Round.tie_to_pos_infinity
  ; Unsigned.Round.tie_to_zero
  ; Unsigned.Round.tie_away_from_zero
  ; Unsigned.Round.tie_to_nearest_even
  ; Unsigned.Round.tie_to_nearest_odd
  ]
;;

let test_unsigned_table () =
  for i = 0 to 15 do
    let a = Unsigned.create 2 (Bits.of_int_trunc ~width:5 i) in
    let resize rnd =
      Unsigned.resize ~round:rnd a 3 0 |> Unsigned.signal |> Bits.to_int_trunc
    in
    Stdio.printf "%3i %f " i (Unsigned.to_float a);
    List.iter unsigned_rounding_ops ~f:(fun x -> Stdio.printf "%i " (resize x));
    Stdio.printf "\n"
  done
;;

let%expect_test "unsigned tabular" =
  test_unsigned_table ();
  [%expect
    {|
     0 0.000000 0 0 0 0 0 0 0 0 0 0
     1 0.250000 0 1 0 1 0 0 0 0 0 0
     2 0.500000 0 1 0 1 0 1 0 1 0 1
     3 0.750000 0 1 0 1 1 1 1 1 1 1
     4 1.000000 1 1 1 1 1 1 1 1 1 1
     5 1.250000 1 2 1 2 1 1 1 1 1 1
     6 1.500000 1 2 1 2 1 2 1 2 2 1
     7 1.750000 1 2 1 2 2 2 2 2 2 2
     8 2.000000 2 2 2 2 2 2 2 2 2 2
     9 2.250000 2 3 2 3 2 2 2 2 2 2
    10 2.500000 2 3 2 3 2 3 2 3 2 3
    11 2.750000 2 3 2 3 3 3 3 3 3 3
    12 3.000000 3 3 3 3 3 3 3 3 3 3
    13 3.250000 3 4 3 4 3 3 3 3 3 3
    14 3.500000 3 4 3 4 3 4 3 4 4 3
    15 3.750000 3 4 3 4 4 4 4 4 4 4
    |}]
;;

let signed_rounding_ops =
  [ Signed.Round.neg_infinity
  ; Signed.Round.pos_infinity
  ; Signed.Round.to_zero
  ; Signed.Round.away_from_zero
  ; Signed.Round.tie_to_neg_infinity
  ; Signed.Round.tie_to_pos_infinity
  ; Signed.Round.tie_to_zero
  ; Signed.Round.tie_away_from_zero
  ; Signed.Round.tie_to_nearest_even
  ; Signed.Round.tie_to_nearest_odd
  ]
;;

let test_signed_table () =
  for i = -8 to 7 do
    let a = Signed.create 2 (Bits.of_int_trunc ~width:5 i) in
    let resize rnd =
      Signed.resize ~round:rnd a 3 0 |> Signed.signal |> Bits.to_signed_int
    in
    Stdio.printf "%3i %+f " i (Signed.to_float a);
    List.iter signed_rounding_ops ~f:(fun x -> Stdio.printf "%+i " (resize x));
    Stdio.printf "\n"
  done
;;

let%expect_test "signed tabular" =
  test_signed_table ();
  [%expect
    {|
    -8 -2.000000 -2 -2 -2 -2 -2 -2 -2 -2 -2 -2
    -7 -1.750000 -2 -1 -1 -2 -2 -2 -2 -2 -2 -2
    -6 -1.500000 -2 -1 -1 -2 -2 -1 -1 -2 -2 -1
    -5 -1.250000 -2 -1 -1 -2 -1 -1 -1 -1 -1 -1
    -4 -1.000000 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
    -3 -0.750000 -1 +0 +0 -1 -1 -1 -1 -1 -1 -1
    -2 -0.500000 -1 +0 +0 -1 -1 +0 +0 -1 +0 -1
    -1 -0.250000 -1 +0 +0 -1 +0 +0 +0 +0 +0 +0
     0 +0.000000 +0 +0 +0 +0 +0 +0 +0 +0 +0 +0
     1 +0.250000 +0 +1 +0 +1 +0 +0 +0 +0 +0 +0
     2 +0.500000 +0 +1 +0 +1 +0 +1 +0 +1 +0 +1
     3 +0.750000 +0 +1 +0 +1 +1 +1 +1 +1 +1 +1
     4 +1.000000 +1 +1 +1 +1 +1 +1 +1 +1 +1 +1
     5 +1.250000 +1 +2 +1 +2 +1 +1 +1 +1 +1 +1
     6 +1.500000 +1 +2 +1 +2 +1 +2 +1 +2 +2 +1
     7 +1.750000 +1 +2 +1 +2 +2 +2 +2 +2 +2 +2
    |}]
;;

type unsigned_resize_t = (Bits.t, Unsigned.t) Hardcaml.With_valid.t2 [@@deriving sexp_of]
type signed_resize_t = (Bits.t, Signed.t) Hardcaml.With_valid.t2 [@@deriving sexp_of]

let test_resize ~i ~f ~i' ~f' v =
  let fu = Unsigned.create f (Bits.of_int_trunc ~width:(i + f) v) in
  let fu_unsigned_wrap =
    Unsigned.resize_with_valid ~overflow:Unsigned.Overflow.wrap fu i' f'
  in
  let fu_unsigned_saturate =
    Unsigned.resize_with_valid ~overflow:Unsigned.Overflow.saturate fu i' f'
  in
  let fu_round_is_lossless = Unsigned.round_is_lossless fu f' in
  let fs = Signed.create f (Bits.of_int_trunc ~width:(i + f) v) in
  let fs_signed_wrap = Signed.resize_with_valid ~overflow:Signed.Overflow.wrap fs i' f' in
  let fs_signed_saturate =
    Signed.resize_with_valid ~overflow:Signed.Overflow.saturate fs i' f'
  in
  let fs_round_is_lossless = Signed.round_is_lossless fs f' in
  print_s
    [%message
      (fu : Unsigned.t)
        (fu_unsigned_wrap : unsigned_resize_t)
        (fu_unsigned_saturate : unsigned_resize_t)
        (fu_round_is_lossless : Bits.t)
        (fs : Signed.t)
        (fs_signed_wrap : signed_resize_t)
        (fs_signed_saturate : signed_resize_t)
        (fs_round_is_lossless : Bits.t)]
;;

let%expect_test "resize to a larger size" =
  test_resize ~i:1 ~f:1 ~i':2 ~f':2 3;
  [%expect
    {|
    ((fu (
       (s  11)
       (fp 1)))
     (fu_unsigned_wrap (
       (valid 1)
       (value (
         (s  0110)
         (fp 2)))))
     (fu_unsigned_saturate (
       (valid 1)
       (value (
         (s  0110)
         (fp 2)))))
     (fu_round_is_lossless 1)
     (fs (
       (s  11)
       (fp 1)))
     (fs_signed_wrap (
       (valid 1)
       (value (
         (s  1110)
         (fp 2)))))
     (fs_signed_saturate (
       (valid 1)
       (value (
         (s  1110)
         (fp 2)))))
     (fs_round_is_lossless 1))
    |}];
  test_resize ~i:4 ~f:3 ~i':6 ~f':3 0b1111000;
  [%expect
    {|
    ((fu (
       (s  1111000)
       (fp 3)))
     (fu_unsigned_wrap (
       (valid 1)
       (value (
         (s  001111000)
         (fp 3)))))
     (fu_unsigned_saturate (
       (valid 1)
       (value (
         (s  001111000)
         (fp 3)))))
     (fu_round_is_lossless 1)
     (fs (
       (s  1111000)
       (fp 3)))
     (fs_signed_wrap (
       (valid 1)
       (value (
         (s  111111000)
         (fp 3)))))
     (fs_signed_saturate (
       (valid 1)
       (value (
         (s  111111000)
         (fp 3)))))
     (fs_round_is_lossless 1))
    |}];
  test_resize ~i:4 ~f:3 ~i':4 ~f':5 0b1111000;
  [%expect
    {|
    ((fu (
       (s  1111000)
       (fp 3)))
     (fu_unsigned_wrap (
       (valid 1)
       (value (
         (s  111100000)
         (fp 5)))))
     (fu_unsigned_saturate (
       (valid 1)
       (value (
         (s  111100000)
         (fp 5)))))
     (fu_round_is_lossless 1)
     (fs (
       (s  1111000)
       (fp 3)))
     (fs_signed_wrap (
       (valid 1)
       (value (
         (s  111100000)
         (fp 5)))))
     (fs_signed_saturate (
       (valid 1)
       (value (
         (s  111100000)
         (fp 5)))))
     (fs_round_is_lossless 1))
    |}]
;;

let%expect_test "test overflow behavior - resize to smaller number of integer bits." =
  (* Unsigned value is above the new range - wrap just truncates while saturate goes to
     the max positive value. Signed value is within the new range - both wrap and saturate
     maintain the same value. *)
  test_resize ~i:4 ~f:3 ~i':3 ~f':3 0b1111000;
  [%expect
    {|
    ((fu (
       (s  1111000)
       (fp 3)))
     (fu_unsigned_wrap (
       (valid 0)
       (value (
         (s  111000)
         (fp 3)))))
     (fu_unsigned_saturate (
       (valid 0)
       (value (
         (s  111111)
         (fp 3)))))
     (fu_round_is_lossless 1)
     (fs (
       (s  1111000)
       (fp 3)))
     (fs_signed_wrap (
       (valid 1)
       (value (
         (s  111000)
         (fp 3)))))
     (fs_signed_saturate (
       (valid 1)
       (value (
         (s  111000)
         (fp 3)))))
     (fs_round_is_lossless 1))
    |}];
  (* Unsigned value is above the new range - wrap just truncates while saturate goes to
     the max positive value. Signed value is within the new range - both wrap and saturate
     maintain the same value. *)
  test_resize ~i:4 ~f:3 ~i':3 ~f':3 0b1110000;
  [%expect
    {|
    ((fu (
       (s  1110000)
       (fp 3)))
     (fu_unsigned_wrap (
       (valid 0)
       (value (
         (s  110000)
         (fp 3)))))
     (fu_unsigned_saturate (
       (valid 0)
       (value (
         (s  111111)
         (fp 3)))))
     (fu_round_is_lossless 1)
     (fs (
       (s  1110000)
       (fp 3)))
     (fs_signed_wrap (
       (valid 1)
       (value (
         (s  110000)
         (fp 3)))))
     (fs_signed_saturate (
       (valid 1)
       (value (
         (s  110000)
         (fp 3)))))
     (fs_round_is_lossless 1))
    |}];
  (* Unsigned value is within the new range - wrap and saturate maintain the same value.
     Signed value is above the new range - wrap just truncates while saturate goes to the
     max positive value. *)
  test_resize ~i:4 ~f:3 ~i':3 ~f':3 0b0101001;
  [%expect
    {|
    ((fu (
       (s  0101001)
       (fp 3)))
     (fu_unsigned_wrap (
       (valid 1)
       (value (
         (s  101001)
         (fp 3)))))
     (fu_unsigned_saturate (
       (valid 1)
       (value (
         (s  101001)
         (fp 3)))))
     (fu_round_is_lossless 1)
     (fs (
       (s  0101001)
       (fp 3)))
     (fs_signed_wrap (
       (valid 0)
       (value (
         (s  101001)
         (fp 3)))))
     (fs_signed_saturate (
       (valid 0)
       (value (
         (s  011111)
         (fp 3)))))
     (fs_round_is_lossless 1))
    |}];
  (* Unsigned value is above the new range - wrap just truncates and saturate goes to the
     max positive value. Signed value is below the new range - wrap just truncates and
     saturate goes to the max negative value. *)
  test_resize ~i:4 ~f:3 ~i':3 ~f':3 0b1000000;
  [%expect
    {|
    ((fu (
       (s  1000000)
       (fp 3)))
     (fu_unsigned_wrap (
       (valid 0)
       (value (
         (s  000000)
         (fp 3)))))
     (fu_unsigned_saturate (
       (valid 0)
       (value (
         (s  111111)
         (fp 3)))))
     (fu_round_is_lossless 1)
     (fs (
       (s  1000000)
       (fp 3)))
     (fs_signed_wrap (
       (valid 0)
       (value (
         (s  000000)
         (fp 3)))))
     (fs_signed_saturate (
       (valid 0)
       (value (
         (s  100000)
         (fp 3)))))
     (fs_round_is_lossless 1))
    |}];
  (* Test zero for completeness - signed and unsigned maintain value. *)
  test_resize ~i:4 ~f:3 ~i':3 ~f':3 0b0000000;
  [%expect
    {|
    ((fu (
       (s  0000000)
       (fp 3)))
     (fu_unsigned_wrap (
       (valid 1)
       (value (
         (s  000000)
         (fp 3)))))
     (fu_unsigned_saturate (
       (valid 1)
       (value (
         (s  000000)
         (fp 3)))))
     (fu_round_is_lossless 1)
     (fs (
       (s  0000000)
       (fp 3)))
     (fs_signed_wrap (
       (valid 1)
       (value (
         (s  000000)
         (fp 3)))))
     (fs_signed_saturate (
       (valid 1)
       (value (
         (s  000000)
         (fp 3)))))
     (fs_round_is_lossless 1))
    |}]
;;

let%expect_test "test overflow behavior - resize number of fractional bits." =
  (* All cases are not exact because we are dropping a bit at the bottom. *)
  test_resize ~i:4 ~f:3 ~i':4 ~f':2 0b0000111;
  [%expect
    {|
    ((fu (
       (s  0000111)
       (fp 3)))
     (fu_unsigned_wrap (
       (valid 1)
       (value (
         (s  000011)
         (fp 2)))))
     (fu_unsigned_saturate (
       (valid 1)
       (value (
         (s  000011)
         (fp 2)))))
     (fu_round_is_lossless 0)
     (fs (
       (s  0000111)
       (fp 3)))
     (fs_signed_wrap (
       (valid 1)
       (value (
         (s  000011)
         (fp 2)))))
     (fs_signed_saturate (
       (valid 1)
       (value (
         (s  000011)
         (fp 2)))))
     (fs_round_is_lossless 0))
    |}];
  (* All of the cases are exact because the lowest bit is a 0. *)
  test_resize ~i:4 ~f:3 ~i':4 ~f':2 0b0000110;
  [%expect
    {|
    ((fu (
       (s  0000110)
       (fp 3)))
     (fu_unsigned_wrap (
       (valid 1)
       (value (
         (s  000011)
         (fp 2)))))
     (fu_unsigned_saturate (
       (valid 1)
       (value (
         (s  000011)
         (fp 2)))))
     (fu_round_is_lossless 1)
     (fs (
       (s  0000110)
       (fp 3)))
     (fs_signed_wrap (
       (valid 1)
       (value (
         (s  000011)
         (fp 2)))))
     (fs_signed_saturate (
       (valid 1)
       (value (
         (s  000011)
         (fp 2)))))
     (fs_round_is_lossless 1))
    |}];
  (* All of the cases are exact because we are increasing the number of fractional bits.
  *)
  test_resize ~i:4 ~f:3 ~i':4 ~f':4 0b0000111;
  [%expect
    {|
    ((fu (
       (s  0000111)
       (fp 3)))
     (fu_unsigned_wrap (
       (valid 1)
       (value (
         (s  00001110)
         (fp 4)))))
     (fu_unsigned_saturate (
       (valid 1)
       (value (
         (s  00001110)
         (fp 4)))))
     (fu_round_is_lossless 1)
     (fs (
       (s  0000111)
       (fp 3)))
     (fs_signed_wrap (
       (valid 1)
       (value (
         (s  00001110)
         (fp 4)))))
     (fs_signed_saturate (
       (valid 1)
       (value (
         (s  00001110)
         (fp 4)))))
     (fs_round_is_lossless 1))
    |}]
;;
