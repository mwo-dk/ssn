module getRangeTest

open Xunit
open FsCheck
open FsCheck.Xunit
open SSN.StringHelpers
open TestHelpers

[<Fact>]
[<Trait("Category", "Unit")>]
let ``getRange of empty string works``() = 
  match getNonWhiteSpaceBoundary "" with
  | Unknown -> assertSuccess()
  | _ -> assertFail()

[<Fact>]
[<Trait("Category", "Unit")>]
let ``getRange of white space string works``() = 
  getRange "   \t   " |> assertEqual Unknown

[<Property>]
[<Trait("Category", "Unit")>]
let ``getRange of non-white space string works``(x: NonWhiteSpaceString) = 
  Unknown <> getRange x.Get

[<Fact>]
[<Trait("Category", "Unit")>]
let ``getRange works 1``() = 
  match getRange " joe " with
  | Known (first, last) -> 
    assertEqual 1 first
    assertEqual 3 last
  | _ -> assertFail()

[<Fact>]
[<Trait("Category", "Unit")>]
let ``getRange works 2``() = 
  match getRange " jo e " with
  | Known (first, last) -> 
    assertEqual 1 first
    assertEqual 4 last
  | _ -> assertFail()