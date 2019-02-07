module getMMTest

open System
open Xunit
open FsCheck.Xunit
open SSN.StringHelpers
open SSN.DA
open TestHelpers

[<Property>]
[<Trait("Category", "Unit")>]
let ``getMM works``(x: DateTimeOffset) = 
  let s = x |> formatDate
  match s |> getMM with
  | Value N -> N = x.Month
  | _ -> false