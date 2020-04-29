module getValue_Test

open Xunit
open FsCheck
open FsCheck.Xunit
open SSN.StringHelpers

[<Property>]
[<Trait("Category", "Unit")>]
let ``getValue' with empty index list returns accumulated value``(acc: IntValue,x: string) =
  acc = getValue' acc [] x 

[<Property>]
[<Trait("Category", "Unit")>]
let ``getValue' with no value yet and singleton index list returns result of getNthDigit``(n: int,x: string) =
  x |> getNthDigit n = getValue' NoValue [n] x 

[<Property>]
[<Trait("Category", "Unit")>]
let ``getValue' with value and singleton index list works``(acc: int, n: int,x: string) =
  match x |> getValue' (Value acc) [n], x |> getNthDigit n with
  | Value acc', Value N -> acc' = 10*acc+N
  | NoValue, NoValue -> true
  | _ -> false

[<Property>]
[<Trait("Category", "Unit")>]
let ``getValue' with no value yet and non-singleton index list returns result of getNthDigit``(n: int,l: NonEmptyArray<int>,x: string) =
  let tail = l.Get |> Array.toList
  let l = [n] @ tail
  match x |> getValue' NoValue l, x |> getNthDigit n with
  | Value acc, Value N -> Value acc = (x |> getValue' (Value acc) tail)
  | NoValue, NoValue -> true
  | _ -> false

