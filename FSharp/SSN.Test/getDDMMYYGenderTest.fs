module getDDMMYYGenderTest

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open SSN.Common
open SSN.Date
open SSN.DA
open TestHelpers

let getSSN dd mm yy dash (controlCode: NonNegativeInt) =
  let c = (controlCode.Get % 10000)
  if dash then (sprintf "%02i%02i%02i-%04i" dd mm yy c), c
  else (sprintf "%02i%02i%02i%04i" dd mm yy c), c

[<Property>]
[<Trait("Category", "Unit")>]
let ``getDDMMYYGender works``(dd: PositiveInt, mm: PositiveInt, yy: PositiveInt, dash: bool, controlCode: NonNegativeInt, repair: bool) = 
  let yy' = (yy.Get % 10000) + 1
  let mm' = (mm.Get % 12) + 1
  let daysInMonth = DateTime.DaysInMonth(yy', mm')
  let dd' = 1 + (dd.Get % daysInMonth)
  let dd'' = if repair then dd' + 60 else dd'
  let ssn, c = getSSN dd'' mm' yy' dash controlCode
  match ssn |> getDDMMYYGender repair with
  | DDMMYYCCGenderSuccess (dd, mm, yy, gender) ->
    let genderOk = (if c % 2 = 0 then gender = Female else gender = Male)
    dd = dd' && (mm |> monthToInt) = mm' && (yy % 100 = yy') && (if c % 2 = 0 then gender = Female else gender = Male)
  | DDMMYYCCGenderError reason -> false