module utils

open System
open System.IO
open System.Text.RegularExpressions

[<Struct>]
type ProblemInput = { Label: string; Path: string }

let stringNotEmpty (a: String) = a.Length > 0

let getProblemInputs dayLabel : ProblemInput array =
    let inputFiles = Directory.GetFiles $"./{dayLabel}"
                    |> Array.filter (fun a -> a[a.Length - 4 .. a.Length].Equals(".txt"))

    inputFiles
    |> Array.map (fun inputFile -> { Label = Path.GetFileName inputFile; Path = inputFile})

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

#nowarn "3535"

type IProblem =
    static abstract member displaySolution: problemInput: ProblemInput array -> unit
