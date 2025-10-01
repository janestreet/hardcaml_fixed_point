open Base
open Hardcaml

module Make (B : Comb.S) = struct
  open B
  include Utils.Make (B)

  module Round = struct
    type bits = B.t
    type t = int -> bits -> bits

    let neg_infinity fp s = floor fp (se s)
    let pos_infinity fp s = ceil fp (se s)

    let to_zero fp s =
      let sign = msb s in
      mux2 sign (ceil fp (se s)) (floor fp (se s))
    ;;

    let away_from_zero fp s =
      let sign = msb s in
      mux2 sign (floor fp (se s)) (ceil fp (se s))
    ;;

    let tie_to_neg_infinity fp s =
      let half = half fp (se s) in
      ceil fp (se s -: half)
    ;;

    let tie_to_pos_infinity fp s =
      let half = half fp (se s) in
      floor fp (se s +: half)
    ;;

    let tie_to_zero fp s =
      let half = half fp (se s) in
      let sign = msb s in
      mux2 sign (floor fp (se s +: half)) (ceil fp (se s -: half))
    ;;

    let tie_away_from_zero fp s =
      let half = half fp (se s) in
      let sign = msb s in
      mux2 sign (ceil fp (se s -: half)) (floor fp (se s +: half))
    ;;

    let tie_to_nearest_even fp s =
      let half = half fp (se s) in
      let lsb = lsb (get_int fp s) in
      mux2 lsb (floor fp (se s +: half)) (ceil fp (se s -: half))
    ;;

    let tie_to_nearest_odd fp s =
      let half = half fp (se s) in
      let lsb = lsb (get_int fp s) in
      mux2 lsb (ceil fp (se s -: half)) (floor fp (se s +: half))
    ;;

    let generic sel fp s =
      let s = se s in
      let z = zero (width s) in
      let half = half fp s in
      let lsb = lsb (get_int fp s) in
      let sign = msb s in
      let rnd = mux sel [ z; z; z; z; half ] in
      let ceil = ceil fp (s -: rnd) in
      let floor = floor fp (s +: rnd) in
      let sel =
        mux
          sel
          [ vdd
          ; gnd
          ; ~:sign
          ; sign (* directed rounding *)
          ; gnd
          ; vdd
          ; sign
          ; ~:sign
          ; lsb
          ; ~:lsb (* round with tie break *)
          ]
      in
      mux2 sel floor ceil
    ;;
  end

  module Overflow = struct
    type bits = B.t
    type t = int -> int -> bits -> (bits, bits) With_valid.t2

    let apply ~handle fp ib s =
      let i = get_int fp s in
      let f = get_frac fp s in
      if width i = ib
      then { With_valid.value = s; valid = vdd }
      else if width i < ib
      then
        { value =
            With_zero_width.(
              concat_msb [ repeat (Some (msb i)) ~count:(ib - width i); Some i; f ]
              |> to_non_zero_width)
        ; valid = vdd
        }
      else (
        let dropped = i.:[width i - 1, ib] in
        let remaining = i.:[ib - 1, 0] in
        let overflow_n = repeat (msb remaining) ~count:(width dropped) ==: dropped in
        let no_overflow_q =
          With_zero_width.(concat_msb [ Some remaining; f ] |> to_non_zero_width)
        in
        let value =
          (* The only difference in the two overflow types is right here. *)
          match handle with
          | `wrap -> (* ignore any potential overflow and wrap. *) no_overflow_q
          | `saturate ->
            let min = reverse (one (ib + fp)) in
            let max = ~:min in
            let clipped = mux2 overflow_n no_overflow_q (mux2 (msb dropped) min max) in
            clipped
        in
        { value; valid = overflow_n })
    ;;

    let saturate = apply ~handle:`saturate
    let wrap = apply ~handle:`wrap
  end

  let to_float s =
    let fp = 2. **. Float.of_int s.fp in
    let i = Float.of_int (B.to_signed_int s.s) in
    i /. fp
  ;;

  let extend s n =
    if n < 0
    then raise_extend n
    else if n = 0
    then s
    else { s = B.concat_msb [ repeat (B.msb s.s) ~count:n; s.s ]; fp = s.fp }
  ;;

  let select_int s i =
    if i <= 0
    then raise_select_int i
    else (
      let si = int s in
      let wi = width_int s in
      if i <= wi
      then si.:[i - 1, 0]
      else B.concat_msb [ B.repeat (B.msb si) ~count:(i - wi); si ])
  ;;

  let select_frac s f =
    if f < 0
    then raise_select_frac f
    else if f = 0
    then None
    else (
      let wf = width_frac s in
      if wf = 0
      then Some (B.zero f)
      else (
        let sf = frac s in
        if f <= wf
        then Some sf.:[wf - 1, wf - f]
        else B.With_zero_width.concat_msb [ Some sf; B.With_zero_width.zero (f - wf) ]))
  ;;

  let select s i f =
    let i' = select_int s i in
    let f' = select_frac s f in
    create f B.With_zero_width.(concat_msb [ Some i'; f' ] |> to_non_zero_width)
  ;;

  let norm l =
    let i = List.fold l ~init:0 ~f:(fun a b -> max a (B.width (int b))) in
    let f = List.fold l ~init:0 ~f:(fun a b -> max a (B.width (frac b))) in
    List.map l ~f:(fun s -> select s i f)
  ;;

  let norm2 a b =
    let l = norm [ a; b ] in
    match l with
    | [ a; b ] -> a, b
    | _ -> assert false
  ;;

  let of_float ip fp f =
    let fp' = Float.of_int fp in
    let fp' = 2.0 **. fp' in
    create fp (B.of_int_trunc ~width:(ip + fp) (Int.of_float (f *. fp')))
  ;;

  let of_float_round_nearest ip fp f =
    let width = ip - 1 (* sign bit *) + fp in
    let fp' = Float.of_int fp in
    let fp' = 2.0 **. fp' in
    let raw = Float.iround_nearest_exn (f *. fp') in
    match Float.sign_exn f with
    | Neg ->
      let min = -(1 lsl width) in
      create fp (B.of_int_trunc ~width:(ip + fp) (Int.max raw min))
    | Pos | Zero ->
      let max = (1 lsl width) - 1 in
      create fp (B.of_int_trunc ~width:(ip + fp) (Int.min raw max))
  ;;

  (* basic arithmetic *)

  let ( +: ) a b =
    let a, b = norm2 a b in
    let a, b = extend a 1, extend b 1 in
    { s = B.( +: ) a.s b.s; fp = a.fp }
  ;;

  let ( -: ) a b =
    let a, b = norm2 a b in
    let a, b = extend a 1, extend b 1 in
    { s = B.( -: ) a.s b.s; fp = a.fp }
  ;;

  let ( *: ) a b = { s = B.( *+ ) a.s b.s; fp = a.fp + b.fp }

  (* comparison *)
  let ( ==: ) a b =
    let a, b = norm2 a b in
    B.( ==: ) a.s b.s
  ;;

  let ( <>: ) a b =
    let a, b = norm2 a b in
    B.( <>: ) a.s b.s
  ;;

  let ( <: ) a b =
    let a, b = norm2 a b in
    B.( <+ ) a.s b.s
  ;;

  let ( <=: ) a b =
    let a, b = norm2 a b in
    B.( <=+ ) a.s b.s
  ;;

  let ( >: ) a b =
    let a, b = norm2 a b in
    B.( >+ ) a.s b.s
  ;;

  let ( >=: ) a b =
    let a, b = norm2 a b in
    B.( >=+ ) a.s b.s
  ;;

  (* mux *)
  let mux sel l =
    let l = norm l in
    let fp = width_frac (List.hd_exn l) in
    let q = B.mux sel (List.map l ~f:signal) in
    create fp q
  ;;

  let mux2 sel a b = mux sel [ b; a ]
  let scale_pow2 = scale_pow2 ~ex:se

  (* resize with rounding and saturation control *)
  let resize_with_valid ?(round = Round.neg_infinity) ?(overflow = Overflow.wrap) s i f =
    let i' = width_int s in
    let f' = width_frac s in
    (* perform rounding *)
    let s = if f >= f' then select s i' f else create f (round (f' - f) s.s) in
    (* perform overflow control *)
    let%tydi { value; valid } = overflow f i s.s in
    { With_valid.value = create f value; valid }
  ;;

  let resize ?round ?overflow s i f =
    let%tydi { value; valid = _ } = resize_with_valid ?round ?overflow s i f in
    value
  ;;
end
