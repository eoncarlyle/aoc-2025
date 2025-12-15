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
    let globalSolution: Map<int array, int> option ref = ref None
    
    let rec innerSolver
        (globalSolution: Map<int array, int> option ref)
        (target: bool array)
        (selected: Map<int array, int>)
        (current: bool array)
        : Unit =
        if target = current then

            globalSolution.Value <-
                globalSolution.Value
                |> Option.map (fun currSol ->
                    if (pressSum currSol > pressSum selected) then
                        selected
                    else
                        currSol)
                |> Option.orElse (Some selected)
        else
            let eligibleValues =
                Map.toArray selected |> Array.filter (fun entry -> snd entry < 2)

            eligibleValues
            |> Array.iter (fun entry ->
                let (buttons, times) = entry
                let nextSelected = Map.add buttons (times + 1) selected
                let nextCurrent = apply current buttons
                let thisSolution = innerSolver globalSolution target nextSelected nextCurrent

                ())

    let initialSelected: Map<int array, int> =
        machine.Buttons |> Array.map (fun a -> a, 0) |> Map

    let initialCurrent =
        seq { for _ in 1 .. machine.Target.Length -> false } |> Seq.toArray

    innerSolver globalSolution machine.Target initialSelected initialCurrent
    globalSolution

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
            let a = machines |> Array.map solver
            Console.WriteLine a
            ())

    interface IProblem with
        static member displaySolution problemInputs = Problem.displaySolution problemInputs
