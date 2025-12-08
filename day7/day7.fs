module day7

open utils
open System
open System.IO

let getRawGrid path =
    File.ReadAllLines path
    |> Array.filter stringNotEmpty
    |> Array.map _.ToCharArray()


let calculateNextRow (unpaintedNextRow: char array) (lastPipeLocations: int array) =
    // There's probably a better way to do this coppying
    let nextRow = [| for i in 0 .. unpaintedNextRow.Length - 1 -> unpaintedNextRow[i] |]

    let mutable splitSet = Set<int>([])

    let legalAdjacentIndicies (row: char array) location =
        match location with
        | 0 -> [| location + 1 |]
        | a when a = row.Length - 1 -> [| location - 1 |]
        | _ -> [| location - 1; location + 1 |]

    lastPipeLocations
    |> Array.iter (fun pipeLocation ->
        if unpaintedNextRow[pipeLocation] = '^' then
            let indicies = legalAdjacentIndicies unpaintedNextRow pipeLocation
            indicies |> Array.iter (fun index -> nextRow[index] <- '|')
            splitSet <- splitSet.Add pipeLocation
        else
            nextRow[pipeLocation] <- '|'

        ())

    nextRow, splitSet.Count

let solution (rawGrid: char array array) =

    let startingColumn = rawGrid[0] |> Array.findIndex _.Equals('S')

    let modifiedRow =
        seq { for i in 0 .. rawGrid[0].Length - 1 -> if i = startingColumn then '|' else rawGrid[1][i] }
        |> Seq.toArray

    let firstGrid = Array.concat [| [| rawGrid[0] |]; [| modifiedRow |]; rawGrid[2..] |]
    let mutable lastRow = firstGrid[1]

    let mutable splitCount = 0

    for row in [| 2 .. rawGrid.Length - 1 |] do
        let currentPipeLocations =
            lastRow
            |> Array.indexed
            |> Array.filter (fun pair -> (snd pair) = '|')
            |> Array.map fst

        let (nextRow, splitCountIncrement) =
            calculateNextRow firstGrid[row] currentPipeLocations

        splitCount <- splitCount + splitCountIncrement
        lastRow <- nextRow

    splitCount

type Problem() =
    static member displaySolution problemInputs =
        problemInputs
        |> Array.iter (fun problemInput ->
            Console.WriteLine problemInput.Label
            Console.WriteLine("==============")
            getRawGrid problemInput.Path |> solution |> Console.WriteLine
            ())

    interface IProblem with
        static member displaySolution problemInputs = Problem.displaySolution problemInputs
