module day10

open day7
open utils
open System
open System.IO
open System.Diagnostics

type Machine =
    { Target: bool list
      Buttons: int list list }

let getGrid path =
    File.ReadAllLines path |> Array.filter stringNotEmpty


let apply (current: bool list) (incomingButton: int list) =
    current
    |> List.indexed
    |> List.map (fun pair ->
        if List.contains (fst pair) incomingButton then
            not (snd pair)
        else
            snd pair)

let getListTable (selected: int list list) = List.countBy id selected |> List.sort

let solver (machine: Machine) =
    let cache: ref<Map<list<list<int> * int>, int option>> = Map [||] |> ref
    let watch = Stopwatch()
    watch.Start()

    let rec innerSolver
        (cache: ref<Map<list<list<int> * int>, int option>>)
        target
        (selected: int list list)
        current
        : int option =

        let selectedKey = getListTable selected

        if Map.containsKey selectedKey cache.Value then
            cache.Value[getListTable selected]
        elif target = current then
            cache.Value <- Map.add selectedKey (Some selected.Length) cache.Value
            Some selected.Length
        else

            let eligibleButtons =
                machine.Buttons
                |> List.filter (fun button -> List.filter (fun a -> a = button) selected |> List.length < 2)

            let maybeMin = Map.values cache.Value |> Seq.choose id |> Seq.sort |> Seq.tryHead

            if Option.exists (fun min -> selected.Length > min) maybeMin then
                None
            else
                let maybeSolution =
                    eligibleButtons
                    |> List.map (fun button ->
                        let nextSelected = selected @ [ button ]
                        let nextCurrent = apply current button
                        innerSolver cache target nextSelected nextCurrent)
                    |> List.choose id
                    |> List.sort
                    |> List.tryHead

                cache.Value <- Map.add selectedKey maybeSolution cache.Value
                maybeSolution

    let initialSelected: Map<int list, int> =
        machine.Buttons |> List.map (fun a -> a, 0) |> Map

    let initialCurrent =
        seq { for _ in 1 .. machine.Target.Length -> false } |> Seq.toList

    let solution = innerSolver cache machine.Target [] initialCurrent
    watch.Stop()
    Console.WriteLine $"Watch: {watch.Elapsed}"
    solution

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
