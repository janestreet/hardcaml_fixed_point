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
    let a = Unsigned.create 2 (Bits.of_int ~width:5 i) in
    let resize rnd = Unsigned.resize ~round:rnd a 3 0 |> Unsigned.signal |> Bits.to_int in
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
    let a = Signed.create 2 (Bits.of_int ~width:5 i) in
    let resize rnd = Signed.resize ~round:rnd a 3 0 |> Signed.signal |> Bits.to_sint in
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

let%expect_test "resize to a larger size" =
  let test_resize ~i ~f ~i' ~f' v =
    let fu = Unsigned.create f (Bits.of_int ~width:(i + f) v) in
    let fu_unsigned_wrap = Unsigned.resize ~overflow:Unsigned.Overflow.wrap fu i' f' in
    let fu_unsigned_saturate =
      Unsigned.resize ~overflow:Unsigned.Overflow.saturate fu i' f'
    in
    let fs = Signed.create f (Bits.of_int ~width:(i + f) v) in
    let fs_signed_wrap = Signed.resize ~overflow:Signed.Overflow.wrap fs i' f' in
    let fs_signed_saturate = Signed.resize ~overflow:Signed.Overflow.saturate fs i' f' in
    print_s
      [%message
        (fu : Unsigned.t)
          (fu_unsigned_wrap : Unsigned.t)
          (fu_unsigned_saturate : Unsigned.t)
          (fs : Signed.t)
          (fs_signed_wrap : Signed.t)
          (fs_signed_saturate : Signed.t)]
  in
  test_resize ~i:1 ~f:1 ~i':2 ~f':2 3;
  [%expect
    {|
    ((fu                   ((s 11)   (fp 1)))
     (fu_unsigned_wrap     ((s 0110) (fp 2)))
     (fu_unsigned_saturate ((s 0110) (fp 2)))
     (fs                   ((s 11)   (fp 1)))
     (fs_signed_wrap       ((s 1110) (fp 2)))
     (fs_signed_saturate   ((s 1110) (fp 2))))
    |}];
  test_resize ~i:4 ~f:3 ~i':6 ~f':3 0b1111000;
  [%expect
    {|
    ((fu                   ((s 1111000)   (fp 3)))
     (fu_unsigned_wrap     ((s 001111000) (fp 3)))
     (fu_unsigned_saturate ((s 001111000) (fp 3)))
     (fs                   ((s 1111000)   (fp 3)))
     (fs_signed_wrap       ((s 111111000) (fp 3)))
     (fs_signed_saturate   ((s 111111000) (fp 3))))
    |}];
  test_resize ~i:4 ~f:3 ~i':4 ~f':5 0b1111000;
  [%expect
    {|
    ((fu                   ((s 1111000)   (fp 3)))
     (fu_unsigned_wrap     ((s 111100000) (fp 5)))
     (fu_unsigned_saturate ((s 111100000) (fp 5)))
     (fs                   ((s 1111000)   (fp 3)))
     (fs_signed_wrap       ((s 111100000) (fp 5)))
     (fs_signed_saturate   ((s 111100000) (fp 5))))
    |}]
;;
