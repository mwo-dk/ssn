module monthToIntTest

open Xunit
open FsCheck.Xunit
open SSN.Date

[<Property>]
[<Trait("Category", "Unit")>]
let ``monthToInt works``(x: Month) = 
  let result = x |> monthToInt
  match x with
  | January -> result = 1
  | February -> result = 2
  | March -> result = 3
  | April -> result = 4
  | May -> result = 5
  | June -> result = 6
  | July -> result = 7
  | August -> result = 8
  | September -> result = 9
  | October -> result = 10
  | November -> result = 11
  | December -> result = 12
