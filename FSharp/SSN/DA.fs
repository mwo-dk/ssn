module SSN.DA

open System
open SSN.Common
open SSN.Date
open SSN.StringHelpers

/// <summary>
/// Extracts the raw 'dd' (day in month) of the birthday part of the string
/// </summary>
/// <param name="x">The source string</param>
let ddIndices = [0;1]
let getDD = getValue ddIndices

/// <summary>
/// Extracts the raw 'mm' (month) of the birthday part of the string
/// </summary>
/// <param name="x">The source string</param>
let mmIndices = [2;3]
let getMM = getValue mmIndices

/// <summary>
/// Extracts the raw 'y' (year) of the birthday part of the string
/// </summary>
/// <param name="x">The source string</param>
let yyIndices = [4;5]
let getYY = getValue yyIndices

type ControlCode = int
/// <summary>
/// Extracts the control value of the string
/// </summary>
/// <param name="hasDash">Flag telling whether there is a dash in the string
/// <param name="x">The source string</param>
let controlCodeIndices = [6..9]
let getControlCode = getValue controlCodeIndices

/// <summary>
/// Union case represent possible errors that can occur, when extracting a birth date from a string
/// </summary>
type YearOfBirthError =
| InvalidYear of Year
| InvalidControl of ControlCode
| InvalidYearAndControl of Year*ControlCode
| InvalidYearAndControlCombination

/// <summary>
/// Represents the outcome of the computation of year of birth
/// </summary>
type YearOfBirth =
| YearOfBirthSuccess of Year              // The year of birth is ok and is included
| YearOfBirthError of YearOfBirthError    // The year of birth is wrong

/// <summary>
/// Computes the year of birth according to the rules set up in
/// https://www.cpr.dk/media/17534/personnummeret-i-cpr.pdf
/// </summary>
/// <param name="yy">The 'yy' part from the SSN</param>
/// <param name="control">The control character</param>
let getBirthYear yy control =
  let inRange a b x = a <= x && x <= b
  let yearValid x = x |> inRange 0 99
  let controlValid x = x |> inRange 0 9999
  match yy |> yearValid, control |> controlValid with
  | false, true -> yy |> InvalidYear |> YearOfBirthError
  | true, false -> control |> InvalidControl |> YearOfBirthError
  | false, false -> (yy,control) |> InvalidYearAndControl |> YearOfBirthError
  | true, true ->
    match yy, control with
    | x, y when x |> inRange 0 99 && y |> inRange 0 3999 -> YearOfBirthSuccess <| 1900 + x
    | x, y when x |> inRange 0 36 && y |> inRange 4000 4999 -> YearOfBirthSuccess <| 2000 + x
    | x, y when x |> inRange 37 99 && y |> inRange 4000 4999 -> YearOfBirthSuccess <| 1900 + x
    | x, y when x |> inRange 0 57 && y |> inRange 5000 8999 -> YearOfBirthSuccess <| 2000 + x
    | x, y when x |> inRange 58 99 && y |> inRange 5000 8999 -> YearOfBirthSuccess <| 1800 + x
    | x, y when x |> inRange 0 36 && y |> inRange 9000 9999 -> YearOfBirthSuccess <| 2000 + x
    | x, y when x |> inRange 37 99 && y |> inRange 9000 9999 -> YearOfBirthSuccess <| 1900 + x
    | _ -> YearOfBirthError InvalidYearAndControlCombination

/// <summary>
/// Determines the <see cref="Gender"/> of a person based on a given digit
/// </summary>
/// <param name="x">The digit to determine the <see cref="Gender"/> on</param>
let getGender x = 
  match x % 2 = 0 with
  | true -> Female
  | _ -> Male

type DigitAndDashValidationError =
| DigitAndDashValidationAllWhiteSpace
| DigitAndDashValidationWrongLength
| DigitAndDashValidationInvalidCharacters of bool*bool // First flag is for digits, second is for dash. False meanse error

type DigitAndDashValidationResult =
| DigitAndDashValidationSuccess
| DigitAndDashValidationError of DigitAndDashValidationError

/// <summary>
/// Validates that trimmed string contains only digits where expected as well as a dash
/// on the expected place - if expected
/// </summary>
/// <param name="ssn">The ssn to be checked</param>
let validateDigitsAndDash ssn =
  match ssn |> getNonWhiteSpaceBoundary with
    | Known (first, last) ->
      let length = last - first + 1
      match length with 
      | 10 -> 
        match ssn |> allInts first last with
        | true -> DigitAndDashValidationSuccess
        | _ -> (false, true) |> DigitAndDashValidationInvalidCharacters |> DigitAndDashValidationError
      | 11 -> 
        match (ssn |> allInts first (first + 5)) && ssn |> allInts (first + 7) last, ssn.[first + 6] = '-' with
        | true, true -> DigitAndDashValidationSuccess
        | x, y -> (x, y) |> DigitAndDashValidationInvalidCharacters |> DigitAndDashValidationError
      | _ -> DigitAndDashValidationWrongLength |> DigitAndDashValidationError
    | _ -> DigitAndDashValidationAllWhiteSpace |> DigitAndDashValidationError

/// <summary>
/// Repairs the day in a month according to rules - if <paramref name="repairDayInMonth"/> is set
/// </summary>
/// <param name="repair">Flag telling whether to repair</param>
/// <param name="dd">The day to optionally repair</param>
let repairDayInMonth repair dd =
  if 61 <= dd && repair then dd - 60 else dd

/// <summary>
/// Union case representing possible errors wrt extracting DD, MM, YY and control code of a string
/// </summary>
type DDMMYYCCError =
| DateError of DateError
| YearOfBirthError of YearOfBirthError
| DataError

/// <summary>
/// Represents the outcome of extracting DD, MM, YY and control code of a string
/// </summary>
type DDMMYYCCResult =
| DDMMYYCCSuccess of DayInMonth*Month*Year*ControlCode
| DDMMYYCCError of DDMMYYCCError

/// <summary>
/// Fetches dd, mm, yy and cc of a trimmed ssn based on whether to repair or not
/// </summary>
/// <param name="repair">Flag telling whether to repair day in month</param>
/// <param name="ssn">The ssn to check</param>
let getDDMMYYCC repair ssn =
  let dd = ssn |> getDD
  let mm = ssn |> getMM
  let yy = ssn |> getYY
  let cc = ssn |> getControlCode
  match dd, mm, yy, cc with
  | Value dd, Value mm, Value yy, Value cc ->
    let dd' = dd |> repairDayInMonth repair
    match getBirthYear yy cc with 
    | YearOfBirthSuccess year ->
      match validateDate year mm dd' with
      | DateValidationSuccess (year, month, day) -> (day, month, year, cc) |> DDMMYYCCSuccess
      | DateValidationError error -> error |> DateError |> DDMMYYCCError
    | YearOfBirth.YearOfBirthError error -> error |> YearOfBirthError |> DDMMYYCCError
  | _ -> DataError |> DDMMYYCCError

/// <summary>
/// Represents the outcome of extracting DD, MM, YY and gender of a string
/// </summary>
type DDMMYYGenderResult =
| DDMMYYCCGenderSuccess of DaysInMonth*Month*Year*Gender
| DDMMYYCCGenderError of DDMMYYCCError

/// <summary>
/// Fetches dd, mm, birth year and gender based on whether to repair or not
/// </summary>
/// <param name="repair">Flag telling whether to repair day in month</param>
/// <param name="ssn">The ssn to check</param>
let getDDMMYYGender repair ssn =
  match ssn |> getDDMMYYCC repair with
  | DDMMYYCCSuccess (dd, mm, year, cc) ->
    (dd, mm, year, cc |> getGender) |> DDMMYYCCGenderSuccess
  | DDMMYYCCError error -> error |> DDMMYYCCGenderError

/// <summary>
/// The weights utilized for modula 11 checks
/// </summary>
let weights = [|4;3;2;7;6;5;4;3;2;1|]

/// <summary>
/// Computes the sum of product (utilized to the modula 11 check) of the digits in
/// the provided string - on the locations denoted by the given indices
/// </summary>
/// <param name="x">The string to work on</param>
let sumOfProduct (x: string) =
  match x |> getNDigits 10 with
  | Some digits ->
    ([0..9], digits) ||> List.fold2 (fun acc i j -> acc + weights.[i]*j) 0 |> Value
  | _ -> NoValue
  
/// <summary>
/// Computes modula 11 of a given integer
/// </summary>
/// <param name="x">The number to compute the modula 11 on</param>
let modula11Of x = x % 11

/// <summary>
/// Checkes whether the provided number is modula 11
/// </summary>
/// <param name="x">The number to check</param>
let isModula11 x = 0 = (x |> modula11Of)

/// <summary>
/// Union case representing possible errors wrt validating a Danish ssn string
/// </summary>
type ValidationError =
| Modula11CheckFail
| DateError of DateError
| YearOfBirthError of YearOfBirthError
| DataError

/// <summary>
/// Represents the outcome of a validation attempt
/// </summary>
type ValidationResult =
| ValidationSuccess of int*Month*int*Gender     // dd, mm, yy, controlCode, gender
| ContentError of DigitAndDashValidationError
| ValidationError of ValidationError

/// <summary>
/// Validates whether a given ssn is valid
/// </summary>
/// <param name="useModula11Check">Flag telling whether to perform the old modula 11 check</param>
/// <param name="repair">Flag telling whether to repair the day in month</param>
/// <param name="ssn">The ssn to be validated</param>
let validate' useModula11Check repair ssn =
  match ssn |> validateDigitsAndDash with
  | DigitAndDashValidationSuccess ->
    match ssn |> getDDMMYYGender repair with
    | DDMMYYCCGenderSuccess (day, month, year, gender) -> 
      match useModula11Check with
      | true ->
        match ssn |> sumOfProduct with 
        | Value N -> 
          match N |> isModula11 with
          | true -> (day, month, year, gender) |> ValidationSuccess
          | _ -> Modula11CheckFail |> ValidationError
        | _ -> Modula11CheckFail |> ValidationError
      | _ -> (day, month, year, gender) |> ValidationSuccess
    | DDMMYYCCGenderError error -> 
      match error with
      | DDMMYYCCError.DateError error -> error |> DateError |> ValidationError
      | DDMMYYCCError.DataError -> DataError|> ValidationError
  | DigitAndDashValidationError reason -> reason |> ContentError  

/// <summary>
/// Represents the outcome of a validation
/// </summary>
type ValidationOutcome =
| Ok                                                 // The validation succeeded
| ContentError of DigitAndDashValidationError
| ValidationError of ValidationError                 // The validation failed

/// https://www.cpr.dk/media/17535/erstatningspersonnummerets-opbygning.pdf

/// <summary>
/// Validates a given danish social security number
/// </summary>
/// <param name="useModula11Check">Flag telling whether to use modula 11 check or not</param>
/// <param name="repair">Flag telling whether to repair day in month part of the
/// birthday</param>
/// <param name="ssn">The ssn to be checked</param>
let validate useModula11Check repair ssn =
  match ssn |> validate' useModula11Check repair with
  | ValidationSuccess _ -> Ok
  | ValidationResult.ContentError reason -> reason |> ContentError
  | ValidationResult.ValidationError reason -> reason |> ValidationError

/// <summary>
/// Validates a given danish social security number in the good old boolean fashion
/// <param name="useModula11Check">Flag telling whether to use modula 11 check or not</param>
/// <param name="repairDayInMonth">Flag telling whether to repair day in month part of the
/// birthday</param>
/// <param name="ssn">The ssn to be checked</param>
let isValid useModula11Check repairDayInMonth ssn =
  match ssn |> validate' useModula11Check repairDayInMonth with 
  | ValidationSuccess _ -> true
  | _ -> false

/// <summary>
/// Represents the result of a valid validation result
/// </summary>
type PersonInfo = {
  Gender: Gender;                          // The gender of the person
  DateOfBirth: BirthDay                   // The day of birth of the person
}

/// <summary>
/// Represents the outcome of extracting info about the person behind the SSN
/// </summary>
type SSNResult = 
| Ok of PersonInfo                                   // The extraction succeeded
| ContentError of DigitAndDashValidationError
| ValidationError of ValidationError                 // The validation failed

/// <summary>
/// Computes the <see cref="SSNResult"/> 
/// </summary>
/// <param name="useModula11Check">Flag telling whether to utilize the modula 11 check</param>
/// <param name="repairDayInMonth">Tlag telling whether to repair day in month</param>
/// <param name="ssn">The SSN string</param>
let getPersonInfo useModula11Check repairDayInMonth ssn =
  match ssn |> validate' useModula11Check repairDayInMonth with
  | ValidationSuccess (dd, mm, year, gender) ->
    Ok {Gender = gender; DateOfBirth = {Year = year; Month = mm; Day = dd}}
  | ValidationResult.ContentError reason -> reason |> ContentError
  | ValidationResult.ValidationError reason -> reason |> ValidationError