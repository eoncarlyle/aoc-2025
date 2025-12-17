module day11

open utils
open System
open System.IO
open System.Collections.Generic


let getGrid path =
    File.ReadAllLines path
    |> Array.filter stringNotEmpty
    |> Array.map (fun row ->
        let colonSplit = row.Split(":")
        let key = string colonSplit[0]

        let value =
            colonSplit[1].Split(" ")
            |> Array.map (fun a -> string a)
            |> Array.filter stringNotEmpty
            |> Array.toList

        key, value)
    |> Map

let solver (graph: Map<string, list<string>>) =
    let discovered = HashSet<string>()
    let cache = Dictionary<string, string list list>()
    cache.Add("out", [ [ "out" ] ])

    let rec dfs (currentNode: string) =
        discovered.Add currentNode |> ignore

        if cache.ContainsKey currentNode then
            cache[currentNode]
        else if currentNode.Equals("out") then
            [ [ "out" ] ]
        else
            let undiscoveredPaths =
                graph[currentNode]
                |> List.filter (fun string -> discovered.Contains(string) |> not)
                |> List.map (fun string -> dfs string |> List.map (fun path -> path @ [ currentNode ]))
                |> List.concat
                |> List.distinct

            let discoveredPaths =
                graph[currentNode]
                |> List.filter (fun string -> discovered.Contains(string))
                |> List.map (fun string -> cache[string] |> List.map (fun path -> path @ [ currentNode ]))
                |> List.concat
                |> List.distinct

            let combinedPaths =
                List.concat [ discoveredPaths; undiscoveredPaths ] |> List.distinct

            cache.Add(currentNode, combinedPaths)
            combinedPaths

    string "you" |> dfs |> _.Length

type Problem() =
    static member displaySolution problemInputs =
        problemInputs
        |> Array.iter (fun problemInput ->
            Console.WriteLine problemInput.Label
            Console.WriteLine("==============")
            problemInput.Path |> (getGrid >> solver) |> Console.WriteLine
            ())

    interface IProblem with
        static member displaySolution problemInputs = Problem.displaySolution problemInputs
