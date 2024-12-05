#load "Utils.fsx"

open System.IO

// Input
let input = File.ReadAllLines "input05.txt"

let inputSample =
    """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""
    |> Utils.splitLines

let getOrderings (lines: string[]) =
    lines
    |> Array.takeWhile (fun s -> s.Contains('|'))
    |> Array.map (fun s -> s.Split("|"))
    |> Array.map (fun a -> a[0] |> int, a[1] |> int)

let getUpdates (lines: string[]) (ordLength) =
    lines
    |> Array.skip (ordLength + 1)
    |> Array.map (fun s -> s.Split(",") |> Array.map int)

let isInOrder (update: int[]) (first: int, second: int) =
    let firstIndex = update |> Array.tryFindIndex (fun x -> x = first)
    let secondIndex = update |> Array.tryFindIndex (fun x -> x = second)

    match firstIndex, secondIndex with
    | Some f, Some s -> f < s
    | _ -> true

// Part 1
let isOrdered (orderings: (int * int)[]) (update: int[]) =
    orderings |> Array.forall (isInOrder update)

let getMiddleNumber (update: int[]) =
    let middleInde = (update.Length - 1) / 2
    update.[middleInde]

let sampleSolution1 =
    let orderings = inputSample |> getOrderings
    let updates = getUpdates inputSample orderings.Length

    updates
    |> Array.filter (isOrdered orderings)
    |> Array.map getMiddleNumber
    |> Array.sum


let solution1 =
    let orderings = input |> getOrderings
    let updates = getUpdates input orderings.Length

    updates
    |> Array.filter (isOrdered orderings)
    |> Array.map getMiddleNumber
    |> Array.sum

// Part 2
let sortTwoNumbers (orderings: (int * int)[]) (one: int, two: int) =
    let notSorted = orderings |> Array.contains (two, one)
    if notSorted then (two, one) else (one, two)

let sortUpdate (orderings: (int * int)[]) (update: int[]) =
    let mutable sorted = false
    let mutable arr = update

    while not sorted do
        sorted <- true

        for i = 0 to arr.Length - 2 do
            let pair = (arr.[i], arr.[i + 1])
            let sortedPair = pair |> sortTwoNumbers orderings

            if sortedPair <> pair then
                arr.[i] <- sortedPair |> fst
                arr.[i + 1] <- sortedPair |> snd
                sorted <- false

    arr

let sampleSolution2 =
    let orderings = inputSample |> getOrderings
    let updates = getUpdates inputSample orderings.Length

    updates
    |> Array.filter (not << (isOrdered orderings))
    |> Array.map (sortUpdate orderings)
    |> Array.map getMiddleNumber
    |> Array.sum

let solution2 =
    let orderings = input |> getOrderings
    let updates = getUpdates input orderings.Length

    updates
    |> Array.filter (not << (isOrdered orderings))
    |> Array.map (sortUpdate orderings)
    |> Array.map getMiddleNumber
    |> Array.sum
