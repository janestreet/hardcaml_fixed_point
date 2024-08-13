(* Very quick test module to make sure we can use the types properly. *)
open Base
open Hardcaml
open Expect_test_helpers_base
module Unsigned = Hardcaml_fixed_point.Unsigned (Bits)
module Signed = Hardcaml_fixed_point.Signed (Bits)

let%expect_test "Unsigned.create" =
  (* 1 integer bit *)
  let t = Unsigned.create 0 Bits.vdd in
  print_s [%message (t : Unsigned.t)];
  [%expect
    {|
    (t (
      (s  1)
      (fp 0)))
    |}];
  (* no integer bits *)
  require_does_raise (fun () -> Unsigned.create 1 Bits.vdd);
  [%expect {| "[create] requires at least 1 integer bit" |}];
  (* negative fractional bits dont work *)
  require_does_raise (fun () -> Unsigned.create (-1) Bits.vdd);
  [%expect {| "[create] negative fractional bits not supported" |}]
;;

let%expect_test "Signed.create" =
  (* 1 integer bit *)
  let t = Signed.create 0 Bits.vdd in
  print_s [%message (t : Signed.t)];
  [%expect
    {|
    (t (
      (s  1)
      (fp 0)))
    |}];
  (* no integer bits *)
  require_does_raise (fun () -> Signed.create 1 Bits.vdd);
  [%expect {| "[create] requires at least 1 integer bit" |}];
  (* negative fractional bits dont work *)
  require_does_raise (fun () -> Signed.create (-1) Bits.vdd);
  [%expect {| "[create] negative fractional bits not supported" |}]
;;

let%expect_test "of_float" =
  let show int_bits frac_bits =
    let mx = 1 lsl (int_bits + frac_bits) in
    for i = 0 to mx - 1 do
      let ufp = Float.(of_int i /. (2. ** of_int frac_bits)) in
      let ufx = Unsigned.of_float int_bits frac_bits ufp in
      let sfp = Float.(of_int Int.(i - (mx / 2)) /. (2. ** of_int frac_bits)) in
      let sfx = Signed.of_float int_bits frac_bits sfp in
      Stdio.printf
        "%+.3f |%s| %+.3f |  %+.3f |%s| %+.3f\n"
        (ufp : float)
        (Bits.to_bstr (Unsigned.signal ufx))
        (Unsigned.to_float ufx)
        (sfp : float)
        (Bits.to_bstr (Signed.signal sfx))
        (Signed.to_float sfx)
    done
  in
  show 1 0;
  [%expect
    {|
    +0.000 |0| +0.000 |  -1.000 |1| -1.000
    +1.000 |1| +1.000 |  +0.000 |0| +0.000
    |}];
  show 1 1;
  [%expect
    {|
    +0.000 |00| +0.000 |  -1.000 |10| -1.000
    +0.500 |01| +0.500 |  -0.500 |11| -0.500
    +1.000 |10| +1.000 |  +0.000 |00| +0.000
    +1.500 |11| +1.500 |  +0.500 |01| +0.500
    |}];
  show 2 2;
  [%expect
    {|
    +0.000 |0000| +0.000 |  -2.000 |1000| -2.000
    +0.250 |0001| +0.250 |  -1.750 |1001| -1.750
    +0.500 |0010| +0.500 |  -1.500 |1010| -1.500
    +0.750 |0011| +0.750 |  -1.250 |1011| -1.250
    +1.000 |0100| +1.000 |  -1.000 |1100| -1.000
    +1.250 |0101| +1.250 |  -0.750 |1101| -0.750
    +1.500 |0110| +1.500 |  -0.500 |1110| -0.500
    +1.750 |0111| +1.750 |  -0.250 |1111| -0.250
    +2.000 |1000| +2.000 |  +0.000 |0000| +0.000
    +2.250 |1001| +2.250 |  +0.250 |0001| +0.250
    +2.500 |1010| +2.500 |  +0.500 |0010| +0.500
    +2.750 |1011| +2.750 |  +0.750 |0011| +0.750
    +3.000 |1100| +3.000 |  +1.000 |0100| +1.000
    +3.250 |1101| +3.250 |  +1.250 |0101| +1.250
    +3.500 |1110| +3.500 |  +1.500 |0110| +1.500
    +3.750 |1111| +3.750 |  +1.750 |0111| +1.750
    |}];
  show 1 3;
  [%expect
    {|
    +0.000 |0000| +0.000 |  -1.000 |1000| -1.000
    +0.125 |0001| +0.125 |  -0.875 |1001| -0.875
    +0.250 |0010| +0.250 |  -0.750 |1010| -0.750
    +0.375 |0011| +0.375 |  -0.625 |1011| -0.625
    +0.500 |0100| +0.500 |  -0.500 |1100| -0.500
    +0.625 |0101| +0.625 |  -0.375 |1101| -0.375
    +0.750 |0110| +0.750 |  -0.250 |1110| -0.250
    +0.875 |0111| +0.875 |  -0.125 |1111| -0.125
    +1.000 |1000| +1.000 |  +0.000 |0000| +0.000
    +1.125 |1001| +1.125 |  +0.125 |0001| +0.125
    +1.250 |1010| +1.250 |  +0.250 |0010| +0.250
    +1.375 |1011| +1.375 |  +0.375 |0011| +0.375
    +1.500 |1100| +1.500 |  +0.500 |0100| +0.500
    +1.625 |1101| +1.625 |  +0.625 |0101| +0.625
    +1.750 |1110| +1.750 |  +0.750 |0110| +0.750
    +1.875 |1111| +1.875 |  +0.875 |0111| +0.875
    |}]
;;

let%expect_test "Unsigned int and frac parts" =
  let x = Unsigned.create 3 (Bits.of_string "11000") in
  let int_width, frac_width = Unsigned.width_int x, Unsigned.width_frac x in
  let int_part, frac_part = Unsigned.select_int x 2, Unsigned.select_frac x 3 in
  print_s
    [%message
      (int_width : int)
        (frac_width : int)
        (int_part : Bits.t)
        (frac_part : Bits.With_zero_width.t)];
  [%expect
    {|
    ((int_width  2)
     (frac_width 3)
     (int_part   11)
     (frac_part (000)))
    |}];
  (* increase each part by 1 bit *)
  let int_part, frac_part = Unsigned.select_int x 3, Unsigned.select_frac x 4 in
  print_s
    [%message
      (int_width : int)
        (frac_width : int)
        (int_part : Bits.t)
        (frac_part : Bits.With_zero_width.t)];
  [%expect
    {|
    ((int_width  2)
     (frac_width 3)
     (int_part   011)
     (frac_part (0000)))
    |}];
  (* decrease each part by 1 bit *)
  let int_part, frac_part = Unsigned.select_int x 1, Unsigned.select_frac x 2 in
  print_s
    [%message
      (int_width : int)
        (frac_width : int)
        (int_part : Bits.t)
        (frac_part : Bits.With_zero_width.t)];
  [%expect
    {|
    ((int_width  2)
     (frac_width 3)
     (int_part   1)
     (frac_part (00)))
    |}];
  require_does_raise (fun () -> Unsigned.select_int x 0);
  [%expect {| ("[select_int] number of bits <= 0" (i 0)) |}];
  require_does_raise (fun () -> Unsigned.select_frac x (-1));
  [%expect {| ("[select_frac] number of bits < 0" (f -1)) |}]
;;

let%expect_test "Signed int and frac parts" =
  let x = Signed.create 3 (Bits.of_string "11000") in
  let int_width, frac_width = Signed.width_int x, Signed.width_frac x in
  let int_part, frac_part = Signed.select_int x 2, Signed.select_frac x 3 in
  print_s
    [%message
      (int_width : int)
        (frac_width : int)
        (int_part : Bits.t)
        (frac_part : Bits.With_zero_width.t)];
  [%expect
    {|
    ((int_width  2)
     (frac_width 3)
     (int_part   11)
     (frac_part (000)))
    |}];
  (* increase each part by 1 bit *)
  let int_part, frac_part = Signed.select_int x 3, Signed.select_frac x 4 in
  print_s
    [%message
      (int_width : int)
        (frac_width : int)
        (int_part : Bits.t)
        (frac_part : Bits.With_zero_width.t)];
  [%expect
    {|
    ((int_width  2)
     (frac_width 3)
     (int_part   111)
     (frac_part (0000)))
    |}];
  (* decrease each part by 1 bit *)
  let int_part, frac_part = Signed.select_int x 1, Signed.select_frac x 2 in
  print_s
    [%message
      (int_width : int)
        (frac_width : int)
        (int_part : Bits.t)
        (frac_part : Bits.With_zero_width.t)];
  [%expect
    {|
    ((int_width  2)
     (frac_width 3)
     (int_part   1)
     (frac_part (00)))
    |}];
  require_does_raise (fun () -> Signed.select_int x 0);
  [%expect {| ("[select_int] number of bits <= 0" (i 0)) |}];
  require_does_raise (fun () -> Signed.select_frac x (-1));
  [%expect {| ("[select_frac] number of bits < 0" (f -1)) |}]
;;

let%expect_test "norm" =
  let x = Unsigned.create 2 (Bits.of_string "1001") in
  let y = Unsigned.create 3 (Bits.of_string "101101") in
  print_s [%message (Unsigned.norm [ x; y ] : Unsigned.t list)];
  [%expect
    {|
    ("Unsigned.norm [x; y]" (
      ((s 010010) (fp 3))
      ((s 101101) (fp 3))))
    |}];
  let x = Signed.create 2 (Bits.of_string "1001") in
  let y = Signed.create 3 (Bits.of_string "101101") in
  print_s [%message (Signed.norm [ x; y ] : Signed.t list)];
  [%expect
    {|
    ("Signed.norm [x; y]" (
      ((s 110010) (fp 3))
      ((s 101101) (fp 3))))
    |}]
;;
