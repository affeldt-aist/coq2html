type t = int * int
let make x y = (x, y)
let min = fst
let max = snd
let in_ x (a, b) = a <= x && x <= b
