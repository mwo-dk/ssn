module getGenderTest

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open SSN.Common
open SSN.StringHelpers
open SSN.DA
open TestHelpers

[<Property>]
[<Trait("Category", "Unit")>]
let ``getGender works``(x: DateTimeOffset, dash: bool, controlCode: NonNegativeInt) = 
  let c = (controlCode.Get % 10000)
  let s = if dash then sprintf "%s-%04i" (x |> formatDate) c
          else sprintf "%s%04i" (x |> formatDate) c
  let lastDigit = s.[s.Length-1] |> toInt
  let gender = lastDigit |> getGender

  if c % 2 = 0 then gender = Female
  else gender = Male
  
