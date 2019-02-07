module SSN.StringHelpers

open System

/// <summary>
/// Alias for a curser
/// </summary>
type Cursor = int

/// <summary>
/// Represents a pair of cursors around a string, that needs to be trimmed
/// </summary>
type CursorPair =
| Unknown
| Known of Cursor*Cursor

type IntValue =
| Value of int
| NoValue

/// <summary>
/// Determines if a given character is a space
/// </summary>
/// <param name="x">The character to categorize</param>
let isWhiteSpace = Char.IsWhiteSpace

/// <summary>
/// Short-hand for Char.IsDigit
/// </summary>
let isDigit = Char.IsDigit

/// <summary>
/// Determines the cursor pair (inclusive) around a string (cannot be null), that needs to be trimmed
/// </summary>
/// <param name="n">The current position in the string to evaluate</param>
/// <param name="cursors">The running curser pair (or not found yet)</param>
/// <param name="x">The string to evaluate</param>
let rec getRange' n cursors (x: string) =
  match n, cursors, x.Length with
  | _, _, _ when n < 0 -> Unknown
  | _, _, l when n > l-1 -> cursors
  | _, _, l when n = l-1 ->
    match cursors with
    | Known(first, _) ->
      match x.[n] |> isWhiteSpace with 
      | true -> cursors
      | _ -> Known(first, n)
    | _ -> 
      match x.[n] |> isWhiteSpace with
      | true -> Unknown
      | _ -> Known(n,n)
  | _ ->
    match cursors with
    | Known(first,_) ->
      match x.[n] |> isWhiteSpace with
      | true -> getRange' (n+1) cursors x
      | _ -> getRange' (n+1) (Known(first, n)) x
    | _ ->
      match x.[n] |> isWhiteSpace with 
      | true -> getRange' (n+1) Unknown x
      | _ -> getRange' (n+1) (Known(n,n)) x
        
/// <summary>
/// Determines the cursor pair (inclusive) around a string (cannot be null), that needs to be trimmed
/// </summary>
let getRange = getRange' 0 Unknown

/// <summary>
/// Computes the range of valid character indices - instead of allocating new string when trimming
/// </summary>
/// <param name="x">The string to "trim"</param>
let getNonWhiteSpaceBoundary x =
  match String.IsNullOrWhiteSpace(x) with
  | true -> Unknown
  | _ -> x |> getRange
  
/// <summary>
/// Determines whether all characters in the provided string <paramref name="x"/> given by the
/// range <paramref name="first"/>;<paramref name="last"/> contains only characters
/// </summary>
/// <param name="first">The index of the first character</param>
/// <param name="last">The index of the last character</param>
/// <param name="x">The string to check</param>
let allInts first last (x: string) =
  {first..last} |> Seq.fold (fun acc n -> acc && x.[n] |> isDigit) true

/// <summary>
/// The character '0' represented as integer
/// </summary>
let zeroAsInt = int('0')

/// <summary>
/// Converts a digit character to its corresponding integer value
/// </summary>
/// <param name="x">The character to convert</param>
let toInt x = int x - zeroAsInt

/// <summary>
/// Fetches the n'th digit in a string - if found. Else NoValue
/// </summary>
/// <param name="n">The running digit count. 0 means now where there.</param>
/// <param name="m">The current position in the string to look at</param>
/// <param name="x">The string to evaluate</param>
let rec getNthDigit' n m (x: string) =
  let mth() = 
    let isDigit = x.[m] |> isDigit
    isDigit, if isDigit then x.[m] |> toInt else -1
  match n, x.Length with
  | _, _  when n < 0 -> NoValue
  | _, l when m < 0 || m > l-1 -> NoValue
  | 0, l ->
    match mth() with
    | true, N -> N |> Value
    | _, _ -> x |> getNthDigit' 0 (m+1)
  | _, _ ->
    match mth() with
    | true, _ -> x |> getNthDigit' (n-1) (m+1)
    | _, _ -> x |> getNthDigit' n (m+1)

/// <summary>
/// Fetches the n'th digit in a string - if found. Else NoValue
/// </summary>
let getNthDigit n = getNthDigit' n 0

/// <summary>
/// Fetches the n first digits of a string
/// </summary>The accumulated optional list till now</param>
/// <param name="x">The string to evaluate</param>
let rec getNDigits' n m acc (x: string) =
  let mth() = 
    let isDigit = x.[m] |> isDigit
    isDigit, if isDigit then x.[m] |> toInt else -1
  let append n =
    match acc with
    | Some acc -> acc @ [n] |> Some
    | _ -> [n] |> Some
  match n, x.Length with
  | _, _  when n < 0 -> None
  | _, l when m < 0 || m > l-1 -> None
  | 0, _ ->
    match mth() with
    | true, N -> N |> append
    | _, _ -> x |> getNDigits' 0 (m+1) acc
  | _, _ ->
    match mth() with
    | true, N -> x |> getNDigits' (n-1) (m+1) (N |> append)
    | _, _ -> x |> getNDigits' n (m+1) acc

/// <summary>
/// 
/// </summary>
/// <param name="n"></param>
let getNDigits n = getNDigits' (n-1) 0 None

/// <summary>
/// Computes the integar value based on the digit indices provided
/// </summary>
/// <param name="acc">The accumulated value till now</param>
/// <param name="indices">The list of indices to look at</param>
/// <param name="x">The string to evaluate</param>
let rec getValue' acc indices x =
  match acc, indices with
  | _, [] -> acc
  | NoValue, [n] -> x |> getNthDigit n
  | Value acc, [n] -> 
    match x |> getNthDigit n with
    | Value N -> 10*acc+N |> Value
    | _ -> NoValue
  | NoValue, n::ns -> 
    match x |> getNthDigit n with
    | Value N -> x |> getValue' (Value N) ns
    | _ -> NoValue
  | Value acc, n::ns ->
    match x |> getNthDigit n with
    | Value N -> x |> getValue' (10*acc+N |> Value) ns
    | _ -> NoValue

/// <summary>
/// /// <summary>
/// Computes the integar value based on the digit indices provided
/// </summary>
/// </summary>
let getValue = getValue' NoValue