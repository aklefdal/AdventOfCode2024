open System

let toInts (line: string) =
    line.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int

let splitLines (s: string) =
    s.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)

