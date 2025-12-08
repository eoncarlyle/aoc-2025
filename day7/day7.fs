module day7

open utils
open System
open System.IO
open System.Collections.Generic

let getRawGrid path =
    File.ReadAllLines path
    |> Array.filter stringNotEmpty
    |> Array.map _.ToCharArray()


let calculateNextRow (incomingNextRow: char array) (lastPipeLocations: int array) =
    let nextRow = [| for i in 1 .. incomingNextRow.Length -> '.' |]

    //let mutable splitMap = lastPipeLocations |> Array.map (fun idx -> idx, false) |> Map
    let mutable splitSet = Set<int>([])

    for pair in (Array.indexed incomingNextRow)[.. nextRow.Length - 1] do
        let (currentIndex, currentValue) = pair

        // This is broken because you need to iterate over both 'split by right' and 'split by left' cases.
        // The smarter thing to do is to iterate over the last pipe locations, but that is a tomorrow problem
        if currentValue = '^' then
            nextRow[currentIndex] <- '^' //This wouldn't work if we needed to support '^^' in rows
        elif Array.contains currentIndex lastPipeLocations then
            nextRow[currentIndex] <- '|'
        elif
            currentIndex < nextRow.Length - 1
            && incomingNextRow[currentIndex + 1] = '^'
            && Array.contains (currentIndex + 1) lastPipeLocations
        then
            nextRow[currentIndex] <- '|'
            splitSet <- splitSet.Add(currentIndex + 1)
        elif
            currentIndex > 0
            && incomingNextRow[currentIndex - 1] = '^'
            && Array.contains (currentIndex - 1) lastPipeLocations
        then
            nextRow[currentIndex] <- '|'
            splitSet <- splitSet.Add(currentIndex - 1)

    (String nextRow) + splitSet.Count.ToString() |> Console.WriteLine
    (nextRow, splitSet.Count)

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
