open! Base
open! Base_quickcheck
open! Expect_test_helpers_base
open Hardcaml

module type Fixed_point = Hardcaml_fixed_point.Fixed_point

module Unsigned = Hardcaml_fixed_point.Unsigned (Bits)
module Signed = Hardcaml_fixed_point.Signed (Bits)

let test_of_float_round_nearest
  ~integer_bits
  ~fractional_bits
  examples
  (module Fixed_point : Fixed_point with type bits = Bits.t)
  =
  List.iter examples ~f:(fun f ->
    let one = Bits.of_int_trunc ~width:(integer_bits + fractional_bits) 1 in
    let ulp = Fixed_point.create fractional_bits one in
    let f_hardcaml = Fixed_point.of_float_round_nearest integer_bits fractional_bits f in
    (* Compare the error of the float chosen against that of the value one ULP (unit of
       least precision) above and below.

       The float chosen should be the one with the smallest error. *)
    let f_hi = Fixed_point.( +: ) f_hardcaml ulp in
    let f_lo = Fixed_point.( -: ) f_hardcaml ulp in
    (* Look at the absolute value of the error of each. *)
    let error = Float.( - ) (Fixed_point.to_float f_hardcaml) f |> Float.abs in
    let error_hi = Float.( - ) (Fixed_point.to_float f_hi) f |> Float.abs in
    let error_lo = Float.( - ) (Fixed_point.to_float f_lo) f |> Float.abs in
    if Float.( > ) error error_hi || Float.( > ) error error_lo
    then
      raise_s
        [%message
          "Error! Not the nearest representable float"
            ~float:(f : float)
            ~result:(Fixed_point.to_float f_hardcaml : float)
            ~hi:(Fixed_point.to_float f_hi : float)
            ~lo:(Fixed_point.to_float f_lo : float)])
;;

let%test_unit "[of_float_round_nearest] examples of signed and unsigned 20.7" =
  let integer_bits = 20 in
  let fractional_bits = 7 in
  let examples_non_negative = [ 0.0; Float.sqrt 2.0; 0.1; 0.125; 0.25; 1.0; 10.0 ] in
  let examples_negative = [ -1.0; -0.25; -0.125; -0.1; -.Float.sqrt 2.0 ] in
  test_of_float_round_nearest
    (examples_non_negative @ examples_negative)
    ~integer_bits
    ~fractional_bits
    (module Signed);
  test_of_float_round_nearest
    examples_non_negative
    ~integer_bits
    ~fractional_bits
    (module Unsigned)
;;

let test_quickcheck_of_float_round_nearest
  ~integer_bits
  ~fractional_bits
  ~lo
  ~hi
  (module Fixed_point : Fixed_point with type bits = Bits.t)
  =
  let property x =
    test_of_float_round_nearest ~integer_bits ~fractional_bits [ x ] (module Fixed_point)
  in
  (* raises to signal failure *)
  Test.run_exn
    ~f:property
    (module struct
      type t = float

      let sexp_of_t = Float.sexp_of_t
      let quickcheck_generator = Generator.float_inclusive lo hi
      let quickcheck_shrinker = Shrinker.float
    end)
;;

let quickcheck_signed ~integer_bits ~fractional_bits =
  let width = integer_bits + fractional_bits in
  let max = Bigint.((one lsl Int.O.(integer_bits - 1 + fractional_bits)) - one) in
  let max_value = Signed.create fractional_bits (Bits.of_signed_bigint ~width max) in
  let min = Bigint.neg (Bigint.( + ) max Bigint.one) in
  let min_value = Signed.create fractional_bits (Bits.of_signed_bigint ~width min) in
  test_quickcheck_of_float_round_nearest
    ~lo:(Signed.to_float min_value)
    ~hi:(Signed.to_float max_value)
    ~integer_bits
    ~fractional_bits
    (module Signed)
;;

let quickcheck_unsigned ~integer_bits ~fractional_bits =
  let width = integer_bits + fractional_bits in
  let max = Bigint.((one lsl width) - one) in
  let max_value = Unsigned.create fractional_bits (Bits.of_unsigned_bigint ~width max) in
  test_quickcheck_of_float_round_nearest
    ~lo:0.
    ~hi:(Unsigned.to_float max_value)
    ~integer_bits
    ~fractional_bits
    (module Unsigned)
;;

let%expect_test "[of_float_round_nearest] signed and unsigned quickcheck" =
  quickcheck_signed ~integer_bits:20 ~fractional_bits:8;
  quickcheck_signed ~integer_bits:20 ~fractional_bits:7;
  quickcheck_unsigned ~integer_bits:20 ~fractional_bits:8;
  quickcheck_unsigned ~integer_bits:20 ~fractional_bits:7;
  quickcheck_unsigned ~integer_bits:32 ~fractional_bits:32
;;

let%expect_test "[of_float_round_nearest] signed and unsigned quickcheck on >63 bits" =
  quickcheck_unsigned ~integer_bits:32 ~fractional_bits:32;
  quickcheck_unsigned ~integer_bits:40 ~fractional_bits:40;
  quickcheck_unsigned ~integer_bits:1 ~fractional_bits:65;
  quickcheck_unsigned ~integer_bits:65 ~fractional_bits:1;
  quickcheck_signed ~integer_bits:32 ~fractional_bits:32;
  quickcheck_signed ~integer_bits:40 ~fractional_bits:40;
  quickcheck_signed ~integer_bits:1 ~fractional_bits:65;
  quickcheck_signed ~integer_bits:65 ~fractional_bits:1
;;

let%expect_test "test near float funkiness boundary" =
  test_of_float_round_nearest
    ~integer_bits:45
    ~fractional_bits:10
    [ 7971738947354.999 ]
    (module Unsigned)
;;
