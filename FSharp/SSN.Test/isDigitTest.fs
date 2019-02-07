module isDigitTest

open Xunit
open FsCheck.Xunit
open SSN.StringHelpers

[<Property>]
[<Trait("Category", "Unit")>]
let ``isDigit works``(x: char) = 
  (System.Char.IsDigit x) = (x |> isDigit)