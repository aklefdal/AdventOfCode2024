open System
open System.Text.RegularExpressions

let toInts (line: string) =
    line.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int

let splitLines (s: string) =
    s.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)

let split (separator: string) (s: string) =
    s.Split(separator)

let join (separator: string) (lines: string seq) =
    String.Join(separator, lines)

let regexMatches (pattern: string) (s: string) = Regex.Matches(s, pattern)

let isAllNumeric (s: string) =
    s.ToCharArray() |> Array.forall Char.IsDigit