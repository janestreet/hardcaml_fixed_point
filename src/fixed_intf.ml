module type Round = sig
  type bits
  type t

  (** These are purely directional rounding, to be read as round to the nearest whole
      fraction specified. For example if we were in base 10 and rounding to 1 decimal
      place:

      [neg_infinity]:   1.91 -> 1.9; -1.15 -> -1.2; 1.55 -> 1.5
      [pos_infinity]:   1.91 -> 2.0; -1.15 -> -1.1; 1.55 -> 1.6
      [to_zero]:        1.91 -> 1.9; -1.15 -> -1.1; 1.55 -> 1.5
      [away_from_zero]: 1.91 -> 2.0; -1.15 -> -1.2; 1.55 -> 1.6
  *)

  val neg_infinity : t
  val pos_infinity : t
  val to_zero : t
  val away_from_zero : t

  (** The tie_ functions always round to the nearest whole fraction specified, except in
      the X.5 case which is determined by the function used. [tie_away_from_zero] would
      match round_nearest in software implementations.

      [tie_to_neg_infinity]: 1.91 -> 1.9; -1.15 -> -1.2; 1.55 -> 1.5
      [tie_to_pos_infinity]: 1.91 -> 1.9; -1.15 -> -1.1; 1.55 -> 1.6
      [tie_to_zero]:         1.91 -> 1.9; -1.15 -> -1.1; 1.55 -> 1.5
      [tie_away_from_zero]:  1.91 -> 1.9; -1.15 -> -1.2; 1.55 -> 1.6
      [tie_to_nearest_even]: 1.91 -> 1.9; -1.15 -> -1.2; 1.55 -> 1.6
      [tie_to_nearest_odd]:  1.91 -> 1.9; -1.15 -> -1.1; 1.55 -> 1.5
  *)

  val tie_to_neg_infinity : t
  val tie_to_pos_infinity : t
  val tie_to_zero : t
  val tie_away_from_zero : t
  val tie_to_nearest_even : t
  val tie_to_nearest_odd : t
  val generic : bits -> t
  val eval : t -> int -> bits -> bits
end

module type Overflow = sig
  type bits
  type t

  val wrap : t
  val saturate : t
  val eval : t -> int -> int -> bits -> bits
end

module type Fixed_point = sig
  type bits

  module Round : Round with type bits = bits
  module Overflow : Overflow with type bits = bits

  type t [@@deriving sexp_of]

  (** create a fixed point value. [create f x] will have [f] fractional bits. [width x -
      f] will be the number of integer bits *)
  val create : int -> bits -> t

  (** return the integer part of the value *)
  val int : t -> bits

  (** return the fractional part of the value *)
  val frac : t -> bits

  (** return the underlying bits *)
  val signal : t -> bits

  (** Map over the internal signal. Useful to register fixed point values

      [map t ~f:(reg spec ~enable:vdd)]. *)
  val map : t -> f:(bits -> bits) -> t

  (** number of integer bits *)
  val width_int : t -> int

  (** number of fractional bits *)
  val width_frac : t -> int

  (** convert fixed point value to a float *)
  val to_float : t -> float

  (** [select_int f x] extracts the integer part, and resizes it to x bits.  Bits are
      dropped from the msb down, if required. *)
  val select_int : t -> int -> bits

  (** [select_frac f x] extracts the fractional part, and resizes it to x bits.  Bits
      are dropped from the lsb up, if required. *)
  val select_frac : t -> int -> bits

  (** resizes a fixed type using select_int and select_frac *)
  val select : t -> int -> int -> t

  (** find largest integer and fractional parts in each fixed value, and resize all
      elements to that size *)
  val norm : t list -> t list

  (** same as norm, but for 2 values *)
  val norm2 : t -> t -> t * t

  (** create a fixed value with the given number of integer and fractional bits from the
      floating point value *)
  val of_float : int -> int -> float -> t

  (** adition *)
  val ( +: ) : t -> t -> t

  (** subtraction *)
  val ( -: ) : t -> t -> t

  (** multiplication *)
  val ( *: ) : t -> t -> t

  (** equality *)
  val ( ==: ) : t -> t -> bits

  (** inequality *)
  val ( <>: ) : t -> t -> bits

  (** less than *)
  val ( <: ) : t -> t -> bits

  (** less than or equal to *)
  val ( <=: ) : t -> t -> bits

  (** greater than *)
  val ( >: ) : t -> t -> bits

  (** greater than or equal to *)
  val ( >=: ) : t -> t -> bits

  (** multiplexor *)
  val mux : bits -> t list -> t

  (** [scale_pow2 t x] will compute [t * (2 ^ x)], allowing for multiplication or division
      by a power of 2. Equivalent to a left or right bit shift but also does boundary
      checking and will extend the underlying number of bits if required. *)
  val scale_pow2 : t -> int -> t

  (** [resize x i f] will resize the integer part to have [i] bits, and fractional part
      to have [f] bits.  Rounding and overflow control is applied *)
  val resize : ?round:Round.t -> ?overflow:Overflow.t -> t -> int -> int -> t
end
