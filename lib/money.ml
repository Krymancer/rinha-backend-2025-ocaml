(* Money conversion utilities *)

let float_to_cents (value : float) : int =
  Float.round (value *. 100.0) |> int_of_float

let cents_to_float (value : int) : float =
  float_of_int value /. 100.0
