module day10

open day7
open utils
open System
open System.IO

type Machine =
    { Target: bool list
      Buttons: int list list }

let getGrid path =
    File.ReadAllLines path |> Array.filter stringNotEmpty


let apply (current: bool list) (incomingButtons: int list) =
    current
    |> List.indexed
    |> List.map (fun pair ->
        if List.contains (fst pair) incomingButtons then
            not (snd pair)
        else
            snd pair)

let pressSum (map: Map<int list, int>) = Map.values map |> Seq.sum

let solver (machine: Machine) =
    let cache: ref<Map<Map<int list, int>, int option>> = Map [||] |> ref

    let rec innerSolver
        (cache: ref<Map<Map<int list, int>, int option>>)
        target
        (selected: Map<int list, int>)
        current
        : int option =
        if Map.containsKey selected cache.Value then
            cache.Value[selected]
        elif target = current then
            cache.Value <- Map.add selected (pressSum selected |> Some) cache.Value
            pressSum selected |> Some
        else
            let eligibleValues =
                Map.toArray selected |> Array.filter (fun entry -> snd entry < 2)

            let values = Map.values cache.Value |> Seq.choose id

            let maybeMin =
                if Seq.length values = 0 then
                    Option.None
                else
                    Seq.length values |> Some

            if Option.exists (fun min -> pressSum selected > min) maybeMin then
                None
            else
                let maybeSolution =
                    eligibleValues
                    |> Array.map (fun entry ->
                        let (buttons, times) = entry
                        let nextSelected = Map.add buttons (times + 1) selected
                        let nextCurrent = apply current buttons
                        innerSolver cache target nextSelected nextCurrent)
                    |> Array.choose id
                    |> Array.sort
                    |> Array.tryHead

                cache.Value <- Map.add selected maybeSolution cache.Value
                maybeSolution

    let initialSelected: Map<int list, int> =
        machine.Buttons |> List.map (fun a -> a, 0) |> Map

    let initialCurrent =
        seq { for _ in 1 .. machine.Target.Length -> false } |> Seq.toList

    let a = innerSolver cache machine.Target initialSelected initialCurrent
    Console.WriteLine(Map.keys cache.Value |> Seq.length)
    a

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
        |> Array.toList


    let buttons =
        row.Substring(openButtons, closedButtons - openButtons)
        |> _.Split(" ")
        |> Array.map (fun a -> a.Substring(1, a.Length - 2) |> _.Split(",") |> Array.map int |> Array.toList)
        |> Array.toList

    { Target = target; Buttons = buttons }

type Problem() =
    static member displaySolution problemInputs =
        problemInputs
        |> Array.iter (fun problemInput ->
            Console.WriteLine problemInput.Label
            Console.WriteLine("==============")
            let machines = getGrid problemInput.Path |> Array.map getTarget

            for machine in machines do
                Console.WriteLine(solver machine |> Option.get)

            //let sum = machines |> Array.map (solver >> Option.get) |> Array.sum
            //Console.WriteLine sum
            ())

    interface IProblem with
        static member displaySolution problemInputs = Problem.displaySolution problemInputs
