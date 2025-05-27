Fixed point type
================

# `hardcaml_fixed_point`

A simple fixed point library for Hardcaml.

Numbers are represented in a [I.F] format which provides [I] integer
bits and [F] fractional bits. We require [I>0] and [F>=0].

Seperate modules for Signed and Unsigned fixed point numbers are
implemented.

The basic arithmetic operators are provided which ensure no overflow
is possible and track precision.

Comparisions do not require the same precision for each argument.

The most important function is [resize]. This can increase or decrease
[I] and [F]. When increasing appropriate padding is performed. When
decreasing [I] the result can either [saturate] or [wrap]. When
decreasing [F] a rounding function can be specified:

* `neg_infinity`
* `pos_infinity`
* `to_zero`
* `away_from_zero`
* `tie_to_neg_infinity`
* `tie_to_pos_infinity`
* `tie_to_zero`
* `tie_away_from_zero`
* `tie_to_nearest_even`
* `tie_to_nearest_odd`

Note that [wrap] along with rounding to [neg_infinity] is effectively
just truncating the top and bottom bits and uses no hardware.
