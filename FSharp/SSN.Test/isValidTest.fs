module isValidTest

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open TestHelpers
open SSN.DA

let getSSN (x: DateTimeOffset) dash (controlCode: NonNegativeInt) =
  let c = (controlCode.Get % 10000)
  if dash then (sprintf "%s-%04i" (x |> formatDate) c, c)
  else (sprintf "%s%04i" (x |> formatDate) c, c)

[<Property>]
[<Trait("Category", "Unit")>]
let ``isValid works``(x: DateTimeOffset, dash: bool, controlCode: NonNegativeInt, useModula11Check: bool, repair: bool) = 
  let s, c = getSSN x dash controlCode
  match s |> validate' useModula11Check repair with
  | ValidationSuccess _ ->
    s |> isValid useModula11Check repair
  | ValidationResult.ValidationError reason -> 
    s |> isValid useModula11Check repair |> not
  | _ -> false