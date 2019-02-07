module getYYTest

open System
open Xunit
open FsCheck.Xunit
open SSN.StringHelpers
open SSN.DA
open TestHelpers

[<Property>]
[<Trait("Category", "Unit")>]
let ``getYY works``(x: DateTimeOffset) = 
  let s = x |> formatDate
  match s |> getYY with
  | Value N -> N = (x.Year % 100)
  | _ -> false