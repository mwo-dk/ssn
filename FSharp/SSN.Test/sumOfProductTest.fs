module sumOfProductTest

open Xunit
open SSN.StringHelpers
open SSN.DA
open TestHelpers

let toChar (n: int) = n + 48 |> char
let randomDigitString n = System.String.Concat(Array.init n (fun n -> (randomDigit() |> toChar)))

[<Fact>]
[<Trait("Category", "Unit")>]
let ``weights is an array of length 10``() = 
  assertEqual 10 (weights |> Array.length)

[<Fact>]
[<Trait("Category", "Unit")>]
let ``weights has expected values``() = 
  assertEqual [|4;3;2;7;6;5;4;3;2;1|] weights

[<Fact>]
[<Trait("Category", "Unit")>]
let ``sumOfProduct works 1``() = 
  let testData = randomDigitString 10
  let expected = [|0..9|] |> Array.fold (fun acc n -> acc + weights.[n] * (toInt testData.[n])) 0
  match testData |> sumOfProduct with
  | Value N -> assertEqual expected N
  | _ -> assertFail()

[<Fact>]
[<Trait("Category", "Unit")>]
let ``sumOfProduct works 2``() = 
  let testData = sprintf "%s-%s" (randomDigitString 6) (randomDigitString 4)
  let indices = [|for i in 0..9 do yield if i <= 5 then i else i+1|]
  let expected = [|0..9|] |> Array.fold (fun acc n -> acc + weights.[n] * (toInt testData.[indices.[n]])) 0
  match testData |> sumOfProduct with
  | Value N -> assertEqual expected N
  | _ -> assertFail()