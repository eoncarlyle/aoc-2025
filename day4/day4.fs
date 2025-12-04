module day4

open utils
open System
open System.IO

let grid (path: String) =
    let rawGrid =
        File.ReadAllLines path
        |> Array.filter (fun a -> a.Length > 0)
        |> Array.map (fun a -> a.ToCharArray())
        |> Array.map (fun a -> Array.append a [| '.' |] |> Array.append [| '.' |]) //Padding

    let paddedRow = seq { for i in 1 .. rawGrid[0].Length -> '.' } |> Seq.toArray

    Array.append rawGrid [| paddedRow |] |> Array.append [| paddedRow |]

let legalRoll (grid: char array array) (rowIdx: int) (colIdx: int) =
    let element = grid[rowIdx][colIdx]

    if element.Equals '@' then
        let adjacents =
            seq {
                for innerRowIdx in rowIdx - 1 .. rowIdx + 1 do
                    for innerColIdx in colIdx - 1 .. colIdx + 1 -> grid[innerRowIdx][innerColIdx]
            }

        let count = Seq.filter (fun a -> a.Equals '@') adjacents |> Seq.length
        count < 5
    else
        false

let legalRollCount (grid: char array array) =
    let getRoll = legalRoll grid

    seq {
        for row in 1 .. grid.Length - 1 do
            for col in 0 .. grid[0].Length - 1 -> getRoll row col
    }
    |> Seq.filter id
    |> Seq.length

type Problem() =
    static member displaySolution problemInputs =
        problemInputs
        |> Array.iter (fun problemInput ->
            Console.WriteLine problemInput.Label
            Console.WriteLine("==============")
            Console.WriteLine(grid problemInput.Path |> legalRollCount)

            ())

    interface IProblem with
        static member displaySolution problemInputs = Problem.displaySolution problemInputs
