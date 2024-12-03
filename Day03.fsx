#load "Utils.fsx"

open System
open System.IO
open System.Text.RegularExpressions

// Input
let input = File.ReadAllText "input03.txt"

let inputSample =
    """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"""

// Part 1
let pattern = @"mul\(\d+,\d+\)"

let solve1 (ss: string) =
    ss
    |> Utils.regexMatches pattern
    |> Seq.map (fun m -> m.Value.[4..m.Value.Length-2].Split(","))
    |> Seq.sumBy (fun a -> (a.[0] |> int) * (a.[1] |> int))

let solution1Sample = inputSample |> solve1

let solution1 = input |> solve1

// Part 2
let sample2 = """xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"""

let solve2 (ss: string) = 
    ss.Split("do()")
    |> Array.map (fun s -> s.Split("don't()").[0])
    |> Utils.join "|"
    |> Utils.regexMatches pattern
    |> Seq.map (fun m -> m.Value.[4..m.Value.Length-2].Split(","))
    |> Seq.sumBy (fun a -> (a.[0] |> int) * (a.[1] |> int))

let sampleSolution2 = 
    sample2
    |> solve2

let solution2 =
    input
    |> solve2
