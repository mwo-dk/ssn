module getPersonInfoTest

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open SSN.Date
open SSN.DA

let getSSN (x: DateTimeOffset) dash unrepair (controlCode: NonNegativeInt) =
  let c = (controlCode.Get % 10000)
  let formatDate() =
    let dd = if unrepair then x.Day + 60 else x.Day
    let mm = x.Month
    let yy = (x.Year % 100)
    sprintf "%02i%02i%02i" dd mm yy
  if dash then (sprintf "%s-%04i" (formatDate()) c, c)
  else (sprintf "%s%04i" (formatDate()) c, c)

[<Property>]
[<Trait("Category", "Unit")>]
let ``getPersonInfo works``(x: DateTimeOffset, dash: bool, controlCode: NonNegativeInt, useModula11Check: bool, repairDay: bool) = 
  let s, c = getSSN x dash repairDay controlCode
  let birthYear =
    match getBirthYear (x.Year % 100) c with
    | YearOfBirthSuccess year -> year
    | _ -> x.Year
  match s |> getPersonInfo useModula11Check repairDay with
  | Ok person ->
    let yearOk = person.DateOfBirth.Year = birthYear
    let monthOk = (person.DateOfBirth.Month |> monthToInt) = x.Month
    let dayOk = person.DateOfBirth.Day = x.Day
    let genderOk = (c |> getGender) = person.Gender
    yearOk && monthOk && dayOk && genderOk
  | SSNResult.ContentError reason -> false
  | SSNResult.ValidationError reason ->
    useModula11Check && reason = Modula11CheckFail