module day10

open utils
open System
open System.IO

type Machine =
    { Target: bool array
      Buttons: int64 array array }

let getGrid path =
    File.ReadAllLines path
    |> Array.filter stringNotEmpty

let indicies (row: String) =
    let index char' =
        row.ToCharArray()
        |> Array.indexed
        |> Array.filter (fun a -> char'.Equals(snd a))
        |> Array.map fst
        |> Array.head

    let openSquare = index '['
    let closedSquare = index ']'
    let openButtons = closedSquare + 2
    let closedButtons = index '{' - 1

    let target =
        row.Substring(openSquare + 1, closedSquare - openSquare - 2)
        |> _.ToCharArray()
        |> Array.map (function
            | '.' -> false
            | '#' -> true
            | _ as a -> failwith $"Illegal: {a}")


    let buttons = row.Substring(openButtons, closedButtons - openButtons)
                    |> _.Split(" ")
                    |> Array.map (fun a -> a.Substring(1, a.Length - 2) |> _.Split(",") |> Array.map int64 )

    {Target= target; Buttons = buttons}

type Problem() =
    static member displaySolution problemInputs =
        problemInputs
        |> Array.iter (fun problemInput ->
            Console.WriteLine problemInput.Label
            Console.WriteLine("==============")
            Console.WriteLine(getGrid problemInput.Path |> Array.map indicies)
            ())

    interface IProblem with
        static member displaySolution problemInputs = Problem.displaySolution problemInputs
