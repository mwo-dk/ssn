module getRange_Test

open Xunit
open FsCheck
open FsCheck.Xunit
open SSN.StringHelpers
open TestHelpers

[<Property>]
[<Trait("Category", "Unit")>]
let ``getRange' for negative n works``(n: NegativeInt, c: CursorPair, x: NonEmptyString) = 
  Unknown = getRange' (n.Get) c x.Get

[<Property>]
[<Trait("Category", "Unit")>]
let ``getRange' for n beyond end works``(n: NonNegativeInt, c: CursorPair, x: NonEmptyString) = 
  c = getRange' (x.Get.Length + n.Get) c x.Get

[<Property>]
[<Trait("Category", "Unit")>]
let ``getRange' for n at end works``(c: CursorPair, x: NonEmptyString) =
  let char' = x.Get.[x.Get.Length-1]
  let c' = getRange' (x.Get.Length-1) c x.Get
  match c with
  | Known(first, _) ->
    match char' |> isWhiteSpace with 
    | true -> c = c'
    | _ -> c' = Known(first, x.Get.Length-1)
  | _ -> 
    match char' |> isWhiteSpace with
    | true -> c' = Unknown
    | _ -> c' = Known(x.Get.Length-1,x.Get.Length-1)
  
[<Fact>]
[<Trait("Category", "Unit")>]
let ``getRange' of empty string works``() = 
  match getNonWhiteSpaceBoundary "" with
  | Unknown -> assertSuccess()
  | _ -> assertFail()

[<Property>]
[<Trait("Category", "Unit")>]
let ``getRange' of white space string works``(c: CursorPair) = 
  c = getRange' 0 c "   \t   "

[<Property>]
[<Trait("Category", "Unit")>]
let ``getRange' of non-white space string works``(x: NonWhiteSpaceString) = 
  Unknown <> getRange' 0 Unknown x.Get

[<Fact>]
[<Trait("Category", "Unit")>]
let ``getRange' works 1``() = 
  match getRange' 0 Unknown " joe " with
  | Known (first, last) -> 
    assertEqual 1 first
    assertEqual 3 last
  | _ -> assertFail()

[<Fact>]
[<Trait("Category", "Unit")>]
let ``getRange' works 2``() = 
  match getRange' 0 Unknown " jo e " with
  | Known (first, last) -> 
    assertEqual 1 first
    assertEqual 4 last
  | _ -> assertFail()