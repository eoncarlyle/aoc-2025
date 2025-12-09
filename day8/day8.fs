module day8

open utils
open System
open System.IO

let getGrid path =
    File.ReadAllLines path
    |> Array.filter stringNotEmpty
    |> Array.map (fun row -> row.Split "," |> Array.map int)
    |> Array.map (fun row -> row[0], row[1], row[2])

let getEntry (grid: (int * int * int) array) pt1 pt2 =
    let a, b = grid[pt1], grid[pt2]
    let a0, a1, a2 = a
    let b0, b1, b2 = b

    (a, b),
    Math.Sqrt(
        Math.Pow(float (b0 - a0), 2)
        + Math.Pow(float (b1 - a1), 2)
        + Math.Pow(float (b2 - a2), 2)
    )

let getMap (grid: (int * int * int) array) =
    Map
    <| seq {
        for pt1 in 0 .. grid.Length - 1 do
            for pt2 in pt1 + 1 .. grid.Length - 1 -> getEntry grid pt1 pt2
    }


type Problem() =
    static member displaySolution problemInputs =
        problemInputs
        |> Array.iter (fun problemInput ->
            Console.WriteLine problemInput.Label
            Console.WriteLine("==============")
            getGrid problemInput.Path |> getMap |> Console.WriteLine
            ())

    interface IProblem with
        static member displaySolution problemInputs = Problem.displaySolution problemInputs
