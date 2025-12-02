module day2

open utils
open System
open System.IO

let problemInputs: ProblemInput array =
    [| { Label = "Personal Test"
         Path = "./day2/personal-test.txt" }
       { Label = "AOC Test"
         Path = "./day2/aoc-test.txt" }
       { Label = "Prod"
         Path = "./day2/prod.txt" } |]

let digitsOf n =
    n.ToString() |> Seq.map (fun c -> int64 c - int64 '0') |> Seq.toArray

let isInvalid (n: int64) =
    let digits = digitsOf n

    if digits.Length % 2 <> 0 then
        false
    else
        Array.zip digits[.. (digits.Length / 2) - 1] (digits[digits.Length / 2 ..])
        |> Array.map (fun a -> fst a = snd a)
        |> Array.reduce (fun a b -> a && b)

let rangeSum (p: int64 * int64) =
    let a = seq { fst p .. snd p } |> Seq.filter isInvalid |> Seq.sum
    a

let parse (row: String) =
    row.Split(",")
    |> Array.filter (fun a -> a.Length > 0)
    |> Array.map (fun combination ->

        let a = combination.Split("-") |> Array.map int64

        if a.Length <> 2 then
            failwith $"Illegal State {a.Length}"

        Array.get a 0, Array.get a 1)

type Problem() =
    static member displaySolution problemInputs =
        problemInputs
        |> Array.iter (fun problemInput ->
            Console.WriteLine problemInput.Label
            Console.WriteLine("==============")

            let lines = File.ReadAllLines problemInput.Path

            Console.WriteLine(parse lines[0] |> Array.map rangeSum |> Array.sum)

            ())

    interface IProblem with
        static member displaySolution problemInputs = Problem.displaySolution problemInputs
