exception NegativeInput
exception EmptyList

type 'a thunk = unit -> 'a
type 'a promise_contents = 
| Unevaluated of 'a thunk
| Evaluated of 'a
type 'a promise = 'a promise_contents ref
type 'a stream = Stream of ('a * 'a stream) thunk

type token =
| Literal of int
| Plus
| Minus
| Mul
| Dot
exception TrefoilError of string
