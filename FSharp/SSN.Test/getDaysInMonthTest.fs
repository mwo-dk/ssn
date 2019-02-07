module getDaysInMonthTest

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open SSN.Date
open TestHelpers

[<Property>]
[<Trait("Category", "Unit")>]
let ``getDaysInMonth for negative years works``(year: NegativeInt) =
  match getDaysInMonth (year.Get) 1 with
  | DaysInMonthError error -> error = ((year.Get) |> InvalidYear)
  | _ -> false

[<Fact>]
[<Trait("Category", "Unit")>]
let ``getDaysInMonth for year zero works``() =
  assertEqual (getDaysInMonth 0 1) (0 |> InvalidYear |> DaysInMonthError)

[<Property>]
[<Trait("Category", "Unit")>]
let ``getDaysInMonth for too large year values works``(year: PositiveInt) =
  match getDaysInMonth (9999 + year.Get) 1 with
  | DaysInMonthError error -> error = ((9999 + year.Get) |> InvalidYear)
  | _ -> false

[<Property>]
[<Trait("Category", "Unit")>]
let ``getDaysInMonth for negative months works``(month: NegativeInt) =
  match getDaysInMonth 1 (month.Get) with
  | DaysInMonthError error -> error = ((month.Get) |> InvalidMonth)
  | _ -> false

[<Fact>]
[<Trait("Category", "Unit")>]
let ``getDaysInMonth for month zero works``() =
  assertEqual (getDaysInMonth 1 0) (0 |> InvalidMonth |> DaysInMonthError)

[<Property>]
[<Trait("Category", "Unit")>]
let ``getDaysInMonth for too large month values works``(month: PositiveInt) =
  match getDaysInMonth 1 (12 + month.Get) with
  | DaysInMonthError error -> error = ((12 + month.Get) |> InvalidMonth)
  | _ -> false  

[<Property>]
[<Trait("Category", "Unit")>]
let ``getDaysInMonth works``(year: PositiveInt, month: PositiveInt) =
  let year' = year.Get % 9999 + 1
  let month' = (month.Get % 12) + 1
  let daysInMonth = DateTime.DaysInMonth(year', month')
  match getDaysInMonth year' month' with
  | DaysInMonthSuccess x -> x = daysInMonth
  | _ -> false