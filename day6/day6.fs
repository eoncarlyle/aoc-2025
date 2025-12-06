module day6

open utils
open System
open System.IO

let parse (row: string) =
    row.Split(" ") |> Array.filter stringNotEmpty

let getGrid path =
    File.ReadAllLines path |> Array.filter stringNotEmpty |> Array.map parse

let apply (operator: String) (arguments: string array) =
    if operator.Equals("+") then
        Array.map Int64.Parse arguments |> Array.sum
    elif operator.Equals("*") then
        Array.map Int64.Parse arguments |> Array.reduce (*)
    else
        failwith "Illegal Operator"

let solution (grid: string array array) =
    seq {
        for col in 0 .. grid[0].Length - 1 ->
            seq { for row in 0 .. grid.Length - 1 -> grid[row][col] }
            |> Seq.toArray
    } |> Seq.map (fun col -> apply(col[col.Length - 1]) col[..col.Length - 2] )
    |> Seq.sum

type Problem() =
    static member displaySolution problemInputs =
        problemInputs
        |> Array.iter (fun problemInput ->
            Console.WriteLine problemInput.Label
            Console.WriteLine("==============")
            Console.WriteLine(getGrid problemInput.Path |> solution)
               
            ())
