module day5

open utils
open System
open System.IO
open System.Text.RegularExpressions

[<Struct>]
type IngredientInstruction =
    | FreshRange of double * double
    | Ingredient of double


let (|Double|_|) (str: string) =
    match Double.TryParse str with
    | true, i -> Some i
    | false, _ -> None

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

let parse (row: string) =
    match row with
    | ParseRegex "(\d+)-(\d+)" [ Double first; Double last ] -> IngredientInstruction.FreshRange(first, last) |> Some
    | ParseRegex "(\d+)" [ Double id ] -> IngredientInstruction.Ingredient id |> Some
    | _ -> None

let getInstructions path =
    File.ReadAllLines path |> Array.map parse

let isFresh (ranges: (int * int) array) (ingredient: int) =
    ranges |> Array.filter (fun range ->
           let (low, high) = range
           low <= ingredient && ingredient <= high
        )

type Problem() =
    static member displaySolution problemInputs =
        problemInputs
        |> Array.iter (fun problemInput ->
            Console.WriteLine problemInput.Label
            Console.WriteLine("==============")
            Console.WriteLine(File.ReadAllLines problemInput.Path |> Array.map parse)

            let instructions = getInstructions problemInput.Path

            let ranges =
                instructions
                |> Array.choose (function
                    | Some(IngredientInstruction.FreshRange(a, b)) -> Some(a, b)
                    | _ -> None)

            let ingredients =
                instructions
                |> Array.choose (function
                    | Some(IngredientInstruction.Ingredient a) -> Some a
                    | _ -> None)

            let ingredientCount =
                ingredients
                |> Array.filter (fun ingredient ->
                    0 < (ranges
                         |> Array.filter (fun range ->
                             let (low, high) = range
                             low <= ingredient && ingredient <= high)
                         |> Array.length))
                |> Array.length

            Console.WriteLine ingredientCount
            ())

    interface IProblem with
        static member displaySolution problemInputs = Problem.displaySolution problemInputs
