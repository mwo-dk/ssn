module getNthDigitTest

open Xunit
open FsCheck
open FsCheck.Xunit
open SSN.StringHelpers

[<Property>]
[<Trait("Category", "Unit")>]
let ``getNthDigit works``(x: NonEmptyString) =
  let m = [0..x.Get.Length-1] |> List.tryFind (fun n -> x.Get.[n] |> isDigit)
  let result = getNthDigit 0 x.Get
  match m with
  | Some m ->
    match result with
    | Value N ->
      let c = x.Get.[m]
      c |> isDigit && N = (toInt <| c)
    | _ -> false
  | None ->
    match result with
    | NoValue -> true
    | _ -> false