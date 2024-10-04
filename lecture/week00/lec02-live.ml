

let max ((x: int), (y: int)) : int =
  if x < y then
    y
  else
    x

let avg ((x: int), (y: int)) : int =
  (x + y) / 2


let rec pow ((base: int), (exp: int)) : int =
  if exp = 0 then
    1
  else
    base * pow(base, exp - 1)

let rec foo x = x

let rec f (x: int) : int =
  g(x)

and g (x: int) : int =
  f(2)
