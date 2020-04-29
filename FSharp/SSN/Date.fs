module SSN.Date

open System

/// <summary>
/// Alias for a day in a month
/// </summary>
type DayInMonth = int

/// <summary>
/// Union case for Gregorian calendars
/// </summary>
type Month = 
| January
| February
| March
| April
| May
| June
| July
| August
| September
| October
| November
| December

/// <summary>
/// Converts a Month to an integer value
/// </summary>
/// <param name="x"></param>
let monthToInt x =
  match x with 
  | January -> 1
  | February -> 2
  | March -> 3
  | April -> 4
  | May -> 5
  | June -> 6
  | July -> 7
  | August -> 8
  | September -> 9
  | October -> 10
  | November -> 11
  | December -> 12

/// <summary>
/// Represents the outcome of converting an integer value to a month
/// </summary>
type MonthValue =
| Month of Month
| NoMonth

/// <summary>
/// Converts an integer to a MonthValue
/// </summary>
/// <param name="x"></param>
let intToMonth x =
  match x with
  | 1 -> January |> Month
  | 2 -> February |> Month
  | 3 -> March |> Month
  | 4 -> April |> Month
  | 5 -> May |> Month
  | 6 -> June |> Month
  | 7 -> July |> Month
  | 8 -> August |> Month
  | 9 -> September |> Month
  | 10 -> October |> Month
  | 11 -> November |> Month
  | 12 -> December |> Month
  | _ -> NoMonth

/// <summary>
/// Alias for a year
/// </summary>
type Year = int

/// <summary>
/// Alias for int representing the number of days in a month
/// </summary>
type DaysInMonth = int

/// <summary>
/// Rrepresents error cases with regards to data validation
/// </summary>
type DateError =
| InvalidYear of Year
| InvalidMonth of int
| InvalidYearAndMonth of Year*int
| InvalidDay of DayInMonth

/// <summary>
/// Represents the outcome of retrieving the number of days in a month
/// </summary>
type DaysInMonthResult =
| DaysInMonthSuccess of DaysInMonth
| DaysInMonthError of DateError

/// <summary>
/// Attempts to retrieve the number of days in a given month in a given year
/// </summary>
/// <param name="year">The year to validate. Valid years are in the range [1,9999] </param>
/// <param name="month">The month to validate. Valid months are in the range [1,12] </param>
let getDaysInMonth year month =
  let yearValid = 1 <= year && year <= 9999
  let monthValid = 1 <= month && month <= 12
  match yearValid, monthValid with
  | false, false -> (year, month) |> InvalidYearAndMonth |> DaysInMonthError
  | false, true -> year |> InvalidYear |> DaysInMonthError
  | true, false -> month |> InvalidMonth |> DaysInMonthError
  | _ ->  DateTime.DaysInMonth(year, month) |> DaysInMonthSuccess

/// <summary>
/// Represents the outcome of validating a date
/// </summary>
type DateValidationResult =
| DateValidationSuccess of Year*Month*DayInMonth
| DateValidationError of DateError

/// <summary>
/// Validates a given date
/// </summary>
/// <param name="year">The year of the date to validate. Valid years are in the range [1,9999]</param>
/// <param name="month">The month of the date to validate. Valid months are in the range [1,12]</param>
/// <param name="day">The day in the month of the date to validate. Valid months are in the range [1, <length of month in the year and month provided>]</param>
let validateDate year month day =
  match getDaysInMonth year month with
  | DaysInMonthSuccess monthLength ->
    if 1 <= day && day <= monthLength then 
      match month |> intToMonth with
      | Month month -> (year, month, day) |> DateValidationSuccess
      | _ -> month |> InvalidMonth |> DateValidationError
    else day |> InvalidDay |> DateValidationError
  | DaysInMonthError error -> error |> DateValidationError

/// <summary>
/// Represents a birth day
/// </summary>
type BirthDay = {
  Year: Year;
  Month: Month;
  Day: DayInMonth;
}