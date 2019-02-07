module getNthDigit_Test

open Xunit
open FsCheck
open FsCheck.Xunit
open SSN.StringHelpers

[<Property>]
[<Trait("Category", "Unit")>]
let ``getNthDigit' for negative n works``(n: NegativeInt, m: int) =
  NoValue = getNthDigit' n.Get m ""

[<Property>]
[<Trait("Category", "Unit")>]
let ``getNthDigit' for m beyond end works``(n: NonNegativeInt, m: NonNegativeInt, x: NonEmptyString) =
  NoValue = getNthDigit' n.Get (x.Get.Length + m.Get) x.Get

[<Property>]
[<Trait("Category", "Unit")>]
let ``getNthDigit' when n zero works``(m: NonNegativeInt, x: NonEmptyString) =
  let m' = m.Get % x.Get.Length
  let m'' = [m'..x.Get.Length-1] |> List.tryFind (fun n -> x.Get.[n] |> isDigit)
  let result = getNthDigit' 0 m' x.Get
  match m'' with
  | Some m''' ->
    match result with
    | Value N ->
      let c = x.Get.[m''']
      c |> isDigit && N = (toInt <| c)
    | _ -> false
  | None ->
    match result with
    | NoValue -> true
    | _ -> false