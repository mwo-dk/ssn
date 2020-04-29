module toIntTest

open System
open Xunit
open FsCheck.Xunit
open SSN.StringHelpers
open TestHelpers

[<Fact>]
[<Trait("Category", "Unit")>]
let ``zeroAsInt is 48``() = assertEqual 48 zeroAsInt

[<Fact>]
[<Trait("Category", "Unit")>]
let ``integer characters are converted correctly``() =
  (['0'..'9'], [0..9])||> List.fold2 (fun acc c x -> acc && (toInt c) = x) true

[<Property>]
[<Trait("Category", "Unit")>]
let ``toInt works``(x: char) =
  match Char.IsDigit(x) with
  | true ->  (int(x) - zeroAsInt) = (toInt x)
  | _ -> true