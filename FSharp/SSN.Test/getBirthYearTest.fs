module getBirthYearTest

open Xunit
open FsCheck
open FsCheck.Xunit
open SSN.DA

[<Property>]
[<Trait("Category", "Unit")>]
let ``getBirthYear works``(yy: NonNegativeInt, controlCode: NonNegativeInt) = 
  let yy' = (yy.Get % 100)
  let c = (controlCode.Get % 10000)

  let inRange a b x = a <= x && x <= b
  match getBirthYear yy' c with
  | YearOfBirthSuccess year -> 
    match yy', c with
    | x, y when x |> inRange 0 99 && y |> inRange 0 3999 -> year = 1900 + x
    | x, y when x |> inRange 0 36 && y |> inRange 4000 4999 -> year = 2000 + x
    | x, y when x |> inRange 37 99 && y |> inRange 4000 4999 -> year = 1900 + x
    | x, y when x |> inRange 0 57 && y |> inRange 5000 8999 -> year = 2000 + x
    | x, y when x |> inRange 58 99 && y |> inRange 5000 8999 -> year = 1800 + x
    | x, y when x |> inRange 0 36 && y |> inRange 9000 9999 -> year = 2000 + x
    | x, y when x |> inRange 37 99 && y |> inRange 9000 9999 -> year = 1900 + x
    | _ -> false
  | _ -> false