module validate_Test

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open SSN.DA
open TestHelpers
open SSN.Date

let getSSN (x: DateTimeOffset) dash (controlCode: NonNegativeInt) =
  let c = (controlCode.Get % 10000)
  if dash then sprintf "%s-%04i" (x |> formatDate) c, c
  else (sprintf "%s%04i" (x |> formatDate) c, c)
let getSSNWithNonDigit (x: DateTimeOffset) dash (controlCode: NonNegativeInt) =
  let c = (controlCode.Get % 10000)
  let formatDate' x =
    let x' = x |> formatDate
    sprintf "a%s" <| x'.Substring(1)
  if dash then (sprintf "%s-%04i" (x |> formatDate') c, c)
  else (sprintf "%s%04i" (x |> formatDate') c, c)
let getSSNWithDashFailure (x: DateTimeOffset) (controlCode: NonNegativeInt) =
  let c = (controlCode.Get % 10000)
  (sprintf "%sx%04i" (x |> formatDate) c, c)
let getSSNWithDashFailure' (dd: NonNegativeInt) (mm: NonNegativeInt) (yy: NonNegativeInt) (controlCode: NonNegativeInt) =
  let dd' = (dd.Get % 32)
  let mm' = (mm.Get % 87) + 13
  let yy' = (yy.Get % 100)
  let c = (controlCode.Get % 10000)
  (sprintf "%02i%02i%02i-%04i" dd' mm' yy' c, c)

[<Property>]
[<Trait("Category", "Unit")>]
let ``validate' with non-digit works``(x: DateTimeOffset, dash: bool, controlCode: NonNegativeInt, useModula11Check: bool, repair: bool) = 
  let s, _ = getSSNWithNonDigit x dash controlCode
  match s |> validate' useModula11Check repair with
  | ValidationResult.ContentError reason -> 
    match reason with
    | DigitAndDashValidationInvalidCharacters (digitOk, dashOk) ->
      (digitOk |> not) && dashOk
    | _ -> false
  | _ -> false

[<Property>]
[<Trait("Category", "Unit")>]
let ``validate' with dash failure works``(x: DateTimeOffset, controlCode: NonNegativeInt, useModula11Check: bool, repair: bool) = 
  let s, _ = getSSNWithDashFailure x controlCode
  match s |> validate' useModula11Check repair with
  | ValidationResult.ContentError reason -> 
    match reason with
    | DigitAndDashValidationInvalidCharacters (digitOk, dashOk) ->
      digitOk && (dashOk |> not)
    | _ -> false
  | _ -> false

[<Property>]
[<Trait("Category", "Unit")>]
let ``validate' with month failure works``(dd: NonNegativeInt, mm: NonNegativeInt, yy: NonNegativeInt, controlCode: NonNegativeInt, useModula11Check: bool, repair: bool) = 
  let s, c = getSSNWithDashFailure' dd mm yy controlCode
  match s |> validate' useModula11Check repair with
  | ValidationResult.ValidationError reason -> 
    match reason with
    | DateError error -> 
      match error with
      | InvalidMonth month -> (mm.Get % 87) + 13 = month
      | _ -> false
    | _ -> false
  | _ -> false

[<Property>]
[<Trait("Category", "Unit")>]
let ``validate' works``(x: DateTimeOffset, dash: bool, controlCode: NonNegativeInt, useModula11Check: bool, repair: bool) = 
  let s, c = getSSN x dash controlCode
  match s |> validate' useModula11Check repair with
  | ValidationResult.ContentError reason -> 
    match reason with
    | DigitAndDashValidationInvalidCharacters (digitOk, dashOk) ->
      digitOk && (dashOk |> not)
    | _ -> false
  | _ -> true