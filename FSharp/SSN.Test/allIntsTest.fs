﻿module allIntsTest

open Xunit
open SSN
open SSN.StringHelpers
open TestHelpers

[<Fact>]
[<Trait("Category", "Unit")>]
let ``allInts of string with non-digits works``() = 
  let testData = " 000hatussa666 "
  match getNonWhiteSpaceBoundary testData with
  | Known (first, last) -> assertFalse <| (allInts first last testData)
  | _ -> assertFail()

[<Fact>]
[<Trait("Category", "Unit")>]
let ``allInts of string with digits works``() = 
  let testData = " 0004353453666 "
  match getNonWhiteSpaceBoundary testData with
  | Known (first, last) -> assertTrue <| (allInts first last testData)
  | _ -> assertFail()