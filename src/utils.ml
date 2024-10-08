open Base
open Hardcaml

module Make (B : Comb.S) = struct
  open B

  type bits = B.t [@@deriving sexp_of]
  type with_zero_width = B.With_zero_width.t

  type t =
    { s : B.t
    ; fp : int
    }
  [@@deriving sexp_of]

  let get_int fp s = s.:[width s - 1, fp]
  let get_frac fp s = With_zero_width.select (Some s) ~high:(fp - 1) ~low:0
  let floor = get_int

  let ceil fp s =
    let ib = width s - fp in
    let max_frac =
      With_zero_width.(concat_msb [ zero ib; ones fp ] |> to_non_zero_width)
    in
    get_int fp (s +: max_frac)
  ;;

  let half fp s =
    let ib = width s - fp in
    zero ib @: reverse (one fp)
  ;;

  let create fp s =
    if B.width s <= fp
    then raise_s [%message "[create] requires at least 1 integer bit"]
    else if fp < 0
    then raise_s [%message "[create] negative fractional bits not supported"];
    { s; fp }
  ;;

  let int s = s.s.:[B.width s.s - 1, s.fp]

  let frac s =
    if s.fp < 0
    then raise_s [%message "[frac] fp < 0" (s.fp : int)]
    else if s.fp = 0
    then B.empty
    else s.s.:[s.fp - 1, 0]
  ;;

  let signal s = s.s
  let width_int s = B.width (int s)
  let width_frac s = B.width (frac s)
  let raise_extend n = raise_s [%message "[extend] number of bits < 0" (n : int)]

  let map t ~f =
    let expected_width = B.width t.s in
    let s = f t.s in
    let got_width = B.width s in
    if got_width <> expected_width
    then
      raise_s
        [%message
          "[map] mapped vector is a different width"
            (got_width : int)
            (expected_width : int)];
    { t with s }
  ;;

  let raise_select_int i = raise_s [%message "[select_int] number of bits <= 0" (i : int)]

  let raise_select_frac f =
    raise_s [%message "[select_frac] number of bits < 0" (f : int)]
  ;;

  let rec scale_up_pow2 { s; fp } pow2 =
    if pow2 = 0
    then { s; fp }
    else if fp = 0
    then scale_up_pow2 { s = s @: gnd; fp } (pow2 - 1)
    else scale_up_pow2 { s; fp = fp - 1 } (pow2 - 1)
  ;;

  let rec scale_down_pow2 ~ex { s; fp } pow2 =
    if pow2 = 0
    then { s; fp }
    else if width s - fp = 1
    then scale_down_pow2 ~ex { s = ex s; fp = fp + 1 } (pow2 + 1)
    else scale_down_pow2 ~ex { s; fp = fp + 1 } (pow2 + 1)
  ;;

  let scale_pow2 ~ex x pow2 =
    if pow2 = 0
    then x
    else if pow2 > 0
    then scale_up_pow2 x pow2
    else scale_down_pow2 ~ex x pow2
  ;;

  let round_is_lossless t new_f =
    let old_f = width_frac t in
    if new_f >= old_f then vdd else sel_bottom (frac t) ~width:(old_f - new_f) ==:. 0
  ;;
end
