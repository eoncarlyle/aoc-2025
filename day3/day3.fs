module day3

open utils
open System
open System.IO

let problemInputs: ProblemInput array =
    [| { Label = "Personal Test"
         Path = "./day3/personal-test.txt" }
       { Label = "AOC Test"
         Path = "./day3/aoc-test.txt" }
       { Label = "Prod"
         Path = "./day3/prod.txt" } |]

let digitsOf n =
    n.ToString() |> Seq.map (fun c -> int c - int '0') |> Seq.toArray


let maxJoltage (row: int array) =
    let firstDigitIndex, firstDigit =
        Array.indexed row[.. row.Length - 2] |> Array.maxBy (fun pair -> snd pair)

    let secondDigit = Array.max row[firstDigitIndex + 1 ..]
    firstDigit * 10 + secondDigit

type Problem() =
    static member displaySolution problemInputs =
        problemInputs
        |> Array.iter (fun problemInput ->
            Console.WriteLine problemInput.Label
            Console.WriteLine("==============")

            let solution =
                File.ReadAllLines problemInput.Path
                |> Array.map (digitsOf >> maxJoltage)
                |> Array.sum

            Console.WriteLine solution

            ())

    interface IProblem with
        static member displaySolution problemInputs = Problem.displaySolution problemInputs
