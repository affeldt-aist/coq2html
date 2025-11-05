Definition aiueo := 123.
Definition kakik := true.
Require Import Bool.
Definition eqdec : forall x y: bool, {x = y} + {x <> y} := bool_dec.

Require Import ZArith.

Definition hoge: {y | (y >= 12)%Z}.
  refine (exist _ 12%Z _). now auto with zarith.
Defined.

