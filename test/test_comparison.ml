open! Base
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
  ~op
  =
  let afx = X.of_float width_int_a width_frac_a a in
  let bfx = X.of_float width_int_b width_frac_b b in
  let opfx = op afx bfx in
  Stdio.printf "%f op %f = %s\n" a b (opfx |> Bits.to_bstr)
;;

let%expect_test "(==:)" =
  test (module Unsigned) 2 3 1.25 4 2 1.25 ~op:Unsigned.( ==: );
  [%expect {| 1.250000 op 1.250000 = 1 |}];
  test (module Unsigned) 2 3 1.25 4 2 1.5 ~op:Unsigned.( ==: );
  [%expect {| 1.250000 op 1.500000 = 0 |}];
  test (module Signed) 2 3 1.25 4 2 1.25 ~op:Signed.( ==: );
  [%expect {| 1.250000 op 1.250000 = 1 |}];
  test (module Signed) 2 3 1.25 4 2 1.5 ~op:Signed.( ==: );
  [%expect {| 1.250000 op 1.500000 = 0 |}]
;;

let%expect_test "(<>:)" =
  test (module Unsigned) 2 3 1.25 4 2 1.25 ~op:Unsigned.( <>: );
  [%expect {| 1.250000 op 1.250000 = 0 |}];
  test (module Unsigned) 2 3 1.25 4 2 1.5 ~op:Unsigned.( <>: );
  [%expect {| 1.250000 op 1.500000 = 1 |}];
  test (module Signed) 2 3 1.25 4 2 1.25 ~op:Signed.( <>: );
  [%expect {| 1.250000 op 1.250000 = 0 |}];
  test (module Signed) 2 3 1.25 4 2 1.5 ~op:Signed.( <>: );
  [%expect {| 1.250000 op 1.500000 = 1 |}]
;;

let%expect_test "(<:)" =
  test (module Unsigned) 2 3 1.5 4 2 1.25 ~op:Unsigned.( <: );
  [%expect {| 1.500000 op 1.250000 = 0 |}];
  test (module Unsigned) 2 3 1.25 4 2 1.25 ~op:Unsigned.( <: );
  [%expect {| 1.250000 op 1.250000 = 0 |}];
  test (module Unsigned) 2 3 1.25 4 2 1.5 ~op:Unsigned.( <: );
  [%expect {| 1.250000 op 1.500000 = 1 |}];
  test (module Signed) 2 3 1.5 4 2 1.25 ~op:Signed.( <: );
  [%expect {| 1.500000 op 1.250000 = 0 |}];
  test (module Signed) 2 3 1.25 4 2 1.25 ~op:Signed.( <: );
  [%expect {| 1.250000 op 1.250000 = 0 |}];
  test (module Signed) 2 3 1.25 4 2 1.5 ~op:Signed.( <: );
  [%expect {| 1.250000 op 1.500000 = 1 |}]
;;

let%expect_test "(<=:)" =
  test (module Unsigned) 2 3 1.5 4 2 1.25 ~op:Unsigned.( <=: );
  [%expect {| 1.500000 op 1.250000 = 0 |}];
  test (module Unsigned) 2 3 1.25 4 2 1.25 ~op:Unsigned.( <=: );
  [%expect {| 1.250000 op 1.250000 = 1 |}];
  test (module Unsigned) 2 3 1.25 4 2 1.5 ~op:Unsigned.( <=: );
  [%expect {| 1.250000 op 1.500000 = 1 |}];
  test (module Signed) 2 3 1.5 4 2 1.25 ~op:Signed.( <=: );
  [%expect {| 1.500000 op 1.250000 = 0 |}];
  test (module Signed) 2 3 1.25 4 2 1.25 ~op:Signed.( <=: );
  [%expect {| 1.250000 op 1.250000 = 1 |}];
  test (module Signed) 2 3 1.25 4 2 1.5 ~op:Signed.( <=: );
  [%expect {| 1.250000 op 1.500000 = 1 |}]
;;

let%expect_test "(>:)" =
  test (module Unsigned) 2 3 1.5 4 2 1.25 ~op:Unsigned.( >: );
  [%expect {| 1.500000 op 1.250000 = 1 |}];
  test (module Unsigned) 2 3 1.25 4 2 1.25 ~op:Unsigned.( >: );
  [%expect {| 1.250000 op 1.250000 = 0 |}];
  test (module Unsigned) 2 3 1.25 4 2 1.5 ~op:Unsigned.( >: );
  [%expect {| 1.250000 op 1.500000 = 0 |}];
  test (module Signed) 2 3 1.5 4 2 1.25 ~op:Signed.( >: );
  [%expect {| 1.500000 op 1.250000 = 1 |}];
  test (module Signed) 2 3 1.25 4 2 1.25 ~op:Signed.( >: );
  [%expect {| 1.250000 op 1.250000 = 0 |}];
  test (module Signed) 2 3 1.25 4 2 1.5 ~op:Signed.( >: );
  [%expect {| 1.250000 op 1.500000 = 0 |}]
;;

let%expect_test "(>=:)" =
  test (module Unsigned) 2 3 1.5 4 2 1.25 ~op:Unsigned.( >=: );
  [%expect {| 1.500000 op 1.250000 = 1 |}];
  test (module Unsigned) 2 3 1.25 4 2 1.25 ~op:Unsigned.( >=: );
  [%expect {| 1.250000 op 1.250000 = 1 |}];
  test (module Unsigned) 2 3 1.25 4 2 1.5 ~op:Unsigned.( >=: );
  [%expect {| 1.250000 op 1.500000 = 0 |}];
  test (module Signed) 2 3 1.5 4 2 1.25 ~op:Signed.( >=: );
  [%expect {| 1.500000 op 1.250000 = 1 |}];
  test (module Signed) 2 3 1.25 4 2 1.25 ~op:Signed.( >=: );
  [%expect {| 1.250000 op 1.250000 = 1 |}];
  test (module Signed) 2 3 1.25 4 2 1.5 ~op:Signed.( >=: );
  [%expect {| 1.250000 op 1.500000 = 0 |}]
;;
