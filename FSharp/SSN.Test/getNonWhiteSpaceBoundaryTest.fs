module getNonWhiteSpaceBoundaryTest

open Xunit
open FsCheck
open FsCheck.Xunit
open SSN.StringHelpers
open TestHelpers

[<Fact>]
[<Trait("Category", "Unit")>]
let ``getNonWhiteSpaceBoundary of null string works``() = 
  match getNonWhiteSpaceBoundary null with
  | Unknown -> assertSuccess()
  | _ -> assertFail()

[<Fact>]
[<Trait("Category", "Unit")>]
let ``getNonWhiteSpaceBoundary of empty string works``() = 
  match getNonWhiteSpaceBoundary "" with
  | Unknown -> assertSuccess()
  | _ -> assertFail()

[<Fact>]
[<Trait("Category", "Unit")>]
let ``getNonWhiteSpaceBoundary of white space string works``() = 
  match getNonWhiteSpaceBoundary "    \t   " with
  | Unknown -> assertSuccess()
  | _ -> assertFail()

[<Property>]
[<Trait("Category", "Unit")>]
let ``getNonWhiteSpaceBoundary of non-white space string works``(x: NonWhiteSpaceString) = 
  Unknown <> getNonWhiteSpaceBoundary x.Get

[<Fact>]
[<Trait("Category", "Unit")>]
let ``getNonWhiteSpaceBoundary works 1``() = 
  match getNonWhiteSpaceBoundary " joe " with
  | Known (first, last) -> 
    assertEqual 1 first
    assertEqual 3 last
  | _ -> assertFail()

[<Fact>]
[<Trait("Category", "Unit")>]
let ``getNonWhiteSpaceBoundary works 2``() = 
  match getNonWhiteSpaceBoundary " jo e " with
  | Known (first, last) -> 
    assertEqual 1 first
    assertEqual 4 last
  | _ -> assertFail()