module isWhiteSpaceTest

open Xunit
open FsCheck.Xunit
open SSN.StringHelpers

[<Property>]
[<Trait("Category", "Unit")>]
let ``isWhiteSpace works``(x: char) = 
  (System.Char.IsWhiteSpace x) = (x |> isWhiteSpace)