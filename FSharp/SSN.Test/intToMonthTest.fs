module intToMonthTest

open Xunit
open FsCheck.Xunit
open SSN.Date
open FsCheck

[<Property>]
[<Trait("Category", "Unit")>]
let ``intToMonth for negative values works``(x: NegativeInt) = 
  NoMonth = (x.Get |> intToMonth)

[<Property>]
[<Trait("Category", "Unit")>]
let ``intToMonth for too high values works``(x: PositiveInt) = 
  NoMonth = ((12 + x.Get) |> intToMonth)

[<Property>]
[<Trait("Category", "Unit")>]
let ``intToMonth works``(x: NonNegativeInt) = 
  let x' = (x.Get % 12) + 1
  let result = x' |> intToMonth
  match x' with
  | 1 -> result = Month January
  | 2 -> result = Month February
  | 3 -> result = Month March
  | 4 -> result = Month April
  | 5 -> result = Month May
  | 6 -> result = Month June
  | 7 -> result = Month July
  | 8 -> result = Month August
  | 9 -> result = Month September
  | 10 -> result = Month October
  | 11 -> result = Month November
  | 12 -> result = Month December
  | _ -> false