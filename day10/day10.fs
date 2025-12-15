module day10

open day7
open utils
open System
open System.IO

type Machine =
    { Target: bool array
      Buttons: int array array }

let getGrid path =
    File.ReadAllLines path |> Array.filter stringNotEmpty


let apply (current: bool array) (incomingButtons: int array) =
    current
    |> Array.indexed
    |> Array.map (fun pair ->
        if Array.contains (fst pair) incomingButtons then
            not (snd pair)
        else
            snd pair)

let pressSum (map: Map<int array, int>) = Map.values map |> Seq.sum

let solver (machine: Machine) =
    let cache = ref Map<Map<int array, int>, int>

    let rec innerSolver cache target (selected: Map<int array, int>) current =
        if target = current then
            Some selected
        else
            let eligibleValues =
                Map.toArray selected |> Array.filter (fun entry -> snd entry < 2)

            eligibleValues
            |> Array.map (fun entry ->
                let (buttons, times) = entry
                let nextSelected = Map.add buttons (times + 1) selected
                let nextCurrent = apply current buttons
                innerSolver cache target nextSelected nextCurrent)
            |> Array.choose id
            |> Array.sortBy pressSum
            |> Array.tryHead

    let initialSelected: Map<int array, int> =
        machine.Buttons |> Array.map (fun a -> a, 0) |> Map

    let initialCurrent =
        seq { for _ in 1 .. machine.Target.Length -> false } |> Seq.toArray

    innerSolver cache machine.Target initialSelected initialCurrent

let getTarget (row: String) =
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
        row.Substring(openSquare + 1, closedSquare - openSquare - 1)
        |> _.ToCharArray()
        |> Array.map (function
            | '.' -> false
            | '#' -> true
            | _ as a -> failwith $"Illegal: {a}")


    let buttons =
        row.Substring(openButtons, closedButtons - openButtons)
        |> _.Split(" ")
        |> Array.map (fun a -> a.Substring(1, a.Length - 2) |> _.Split(",") |> Array.map int)

    { Target = target; Buttons = buttons }

type Problem() =
    static member displaySolution problemInputs =
        problemInputs
        |> Array.iter (fun problemInput ->
            Console.WriteLine problemInput.Label
            Console.WriteLine("==============")
            let machines = getGrid problemInput.Path |> Array.map getTarget

            let sum = machines |> Array.map (solver >> Option.get >> pressSum) |> Array.sum

            Console.WriteLine sum
            ())

    interface IProblem with
        static member displaySolution problemInputs = Problem.displaySolution problemInputs
