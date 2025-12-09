module day8

open utils
open System
open System.IO
open System.Collections.Generic

type Point = int * int * int

let getGrid path =
    File.ReadAllLines path
    |> Array.filter stringNotEmpty
    |> Array.map (fun row -> row.Split "," |> Array.map int)
    |> Array.map (fun row -> row[0], row[1], row[2])

let getEntry (grid: (Point) array) pt1 pt2 =
    let a, b = grid[pt1], grid[pt2]
    let a0, a1, a2 = a
    let b0, b1, b2 = b

    (a, b),
    Math.Sqrt(
        Math.Pow(float (b0 - a0), 2)
        + Math.Pow(float (b1 - a1), 2)
        + Math.Pow(float (b2 - a2), 2)
    )

let getDistanceMap (grid: Point array) =
    Map
    <| seq {
        for pt1 in 0 .. grid.Length - 1 do
            for pt2 in pt1 + 1 .. grid.Length - 1 -> getEntry grid pt1 pt2
    }


let bfs (adjacentMap: Map<Point, list<Point>>) (root: Point) =
    let explored = HashSet<Point>()
    explored.Add(root) |> ignore
    let queue = Queue<Point>()
    queue.Enqueue(root)

    while queue.Count > 0 do
        let v = queue.Dequeue()

        adjacentMap[v]
        |> List.iter (fun w ->
            if explored.Contains(w) |> not then
                explored.Add(w) |> ignore
                queue.Enqueue(w)
        )

    explored


let graphContains (adjacentMap: Map<Point, list<Point>>) (root: Point) (target: Point) =
    bfs adjacentMap root |> _.Contains(target)

let getAdjacentMap (grid: (Point) array) : Map<Point, list<Point>> =
    grid |> Array.map (fun a -> a, []) |> Map

let getSolution (grid: Point array) (times: int) =
    let mutable adjacentMap = getAdjacentMap grid
    let distanceMap = getDistanceMap grid

    let sortedDistanceKeys =
        distanceMap |> _.Keys |> Seq.sortBy (fun key -> distanceMap[key]) |> Seq.toList

    for idx in [ 0 .. times - 1 ] do
        let pt1, pt2 = sortedDistanceKeys[idx]

        if adjacentMap[pt1] |> List.contains pt2 |> not then
            adjacentMap <- adjacentMap.Add(pt1, adjacentMap[pt1] @ [ pt2 ])
            adjacentMap <- adjacentMap.Add(pt2, adjacentMap[pt2] @ [ pt1 ])

        ()

    let sizes = HashSet<int>()
    let visited = HashSet<Point>()
     
    adjacentMap.Keys
    |> Seq.iter (fun edge ->
        let cluster = bfs adjacentMap edge
        if visited.Overlaps cluster |> not then
            sizes.Add(cluster.Count) |> ignore
            cluster |> Seq.iter (fun a -> visited.Add(a) |> ignore)
        ())


    sizes |> Seq.sortDescending |> Seq.take 3 |> Seq.reduce (*)


type Problem() =
    static member displaySolution problemInputs =
        problemInputs
        |> Array.iter (fun problemInput ->
            Console.WriteLine problemInput.Label
            Console.WriteLine("==============")
            let grid = getGrid problemInput.Path

            let solution =
                if problemInput.Label.Contains("prod") then
                    getSolution grid 1000
                else
                    getSolution grid 10

            Console.WriteLine solution
            ())

    interface IProblem with
        static member displaySolution problemInputs = Problem.displaySolution problemInputs
