module validateTest

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open SSN.DA
open TestHelpers
open SSN.Common

let getSSN (x: DateTimeOffset) dash (controlCode: NonNegativeInt) =
  let c = (controlCode.Get % 10000)
  if dash then (sprintf "%s-%04i" (x |> formatDate) c, c)
  else (sprintf "%s%04i" (x |> formatDate) c, c)

[<Property>]
[<Trait("Category", "Unit")>]
let ``validate works``(x: DateTimeOffset, dash: bool, controlCode: NonNegativeInt, useModula11Check: bool, repair: bool) = 
  let s, c = getSSN x dash controlCode
  match s |> validate' useModula11Check repair with
  | ValidationSuccess _ ->
    ValidationOutcome.Ok = (s |> validate useModula11Check repair)
  | ValidationResult.ContentError reason ->
    match s |> validate useModula11Check repair with
    | ValidationOutcome.ContentError reason' ->
      reason = reason'
    | _ -> false
  | ValidationResult.ValidationError reason -> 
    match s |> validate useModula11Check repair with
    | ValidationOutcome.ValidationError reason' ->
      reason = reason'
    | _ -> false
  | _ -> false