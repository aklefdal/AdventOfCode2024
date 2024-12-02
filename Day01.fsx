#load "Utils.fsx"

open System
open System.Collections.Generic
open System.IO

// Input
let input =
    File.ReadAllLines @"input01.txt"
    |> Array.map Utils.parseLineOfInts

// Part 1
let list1 = input |> Array.map (fun a -> a.[0]) |> Array.sort
let list2 = input |> Array.map (fun a -> a.[1]) |> Array.sort

let solution1 =
    Array.zip list1 list2 |> Array.map (fun (a, b) -> Math.Abs(a - b)) |> Array.sum

// Part 2
let counts = input |> Array.map (fun a -> a.[1]) |> Array.countBy id |> dict

let similarityIndex (counts: IDictionary<int, int>) (i: int) =
    if counts.ContainsKey(i) then i * counts[i] else 0

let solution2 =
    input
    |> Array.map (fun a -> a.[0])
    |> Array.map (similarityIndex counts)
    |> Array.sum
