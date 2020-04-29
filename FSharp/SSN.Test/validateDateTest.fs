module validateDateTest

open Xunit
open FsCheck.Xunit
open SSN.Date

[<Property>]
[<Trait("Category", "Unit")>]
let ``validateDate works``(year: int, month: int, day: int) =
  match validateDate year month day, getDaysInMonth year month with
  | DateValidationSuccess (_, month', day), DaysInMonthSuccess monthLength ->
    1 <= day && day <= monthLength && month = (month' |> monthToInt)
  | DateValidationError dateError, DaysInMonthSuccess monthLength ->
    day < 1 || monthLength < day
  | DateValidationError dateError, DaysInMonthError dimError ->
    dateError = dimError
  | _ -> false
