﻿module getControlCodeTest

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open SSN.StringHelpers
open SSN.DA
open TestHelpers

[<Property>]
[<Trait("Category", "Unit")>]
let ``getControlCode works``(x: DateTimeOffset, dash: bool, controlCode: NonNegativeInt) = 
  let c = (controlCode.Get % 10000)
  let s = if dash then sprintf "%s-%04i" (x |> formatDate) c
          else sprintf "%s%04i" (x |> formatDate) c
  match s |> getControlCode with
  | Value N -> c = N
  | _ -> false
  