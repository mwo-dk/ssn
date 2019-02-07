module getDDTest

open System
open Xunit
open FsCheck.Xunit
open SSN.StringHelpers
open SSN.DA
open TestHelpers

[<Property>]
[<Trait("Category", "Unit")>]
let ``getDD works``(x: DateTimeOffset) = 
  let s = x |> formatDate
  match s |> getDD with
  | Value N -> N = x.Day
  | _ -> false