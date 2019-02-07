module getDDMMYYCCTest

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open SSN.Date
open SSN.DA

let getSSN dd mm yy dash (controlCode: NonNegativeInt) =
  let c = (controlCode.Get % 10000)
  if dash then (sprintf "%02i%02i%02i-%04i" dd mm yy c), c
  else (sprintf "%02i%02i%02i%04i" dd mm yy c), c

[<Property>]
[<Trait("Category", "Unit")>]
let ``getDDMMYYCC works``(dd: PositiveInt, mm: PositiveInt, yy: PositiveInt, dash: bool, controlCode: NonNegativeInt, repair: bool) = 
  let yy' = (yy.Get % 10000) + 1
  let mm' = (mm.Get % 12) + 1
  let daysInMonth = DateTime.DaysInMonth(yy', mm')
  let dd' = 1 + (dd.Get % daysInMonth)
  let dd'' = if repair then dd' + 60 else dd'
  let ssn, c = getSSN dd'' mm' yy' dash controlCode
  match ssn |> getDDMMYYCC repair with
  | DDMMYYCCSuccess(dd, mm, yy, cc) ->
    match getBirthYear yy' controlCode.Get with
    | YearOfBirthSuccess year -> dd = dd' && (mm |> monthToInt) = mm' && (year % 100) = yy' && cc = controlCode.Get
    | _ -> false
  | _ -> false 