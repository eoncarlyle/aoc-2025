module day9

open utils
open System
open System.IO

type Quadrant =
    | Q1
    | Q2
    | Q3
    | Q4

type Splits =
    { Partitions: (Quadrant * (int64 * int64) array) array
      Marks: (int64 * int64) }

let bruteForce (grid: (int64 * int64) array) =
    grid
    |> Array.map (fun b ->
        grid
        |> Array.map (fun c ->
            (max (fst b) (fst c) - (min (fst b) (fst c)) + 1L)
            * (max (snd b) (snd c) - (min (snd b) (snd c)) + 1L))
        |> Array.max)
    |> Array.max

let getGrid path =
    File.ReadAllLines path
    |> Array.filter stringNotEmpty
    |> Array.map (fun row -> row.Split "," |> Array.map int64)
    |> Array.map (fun row -> row[0], row[1])

let getQuadrantMarks (grid: (int64 * int64) array) =
    grid |> Array.averageBy (fst >> float) |> int, grid |> Array.averageBy (snd >> float) |> int

let splitQuadrant (grid: (int64 * int64) array) =
    let xMark, yMark = getQuadrantMarks grid

    let q1 = grid |> Array.filter (fun pair -> (fst pair) > xMark && (snd pair) > yMark)
    let q2 = grid |> Array.filter (fun pair -> (fst pair) < xMark && (snd pair) > yMark)
    let q3 = grid |> Array.filter (fun pair -> (fst pair) < xMark && (snd pair) < yMark)
    let q4 = grid |> Array.filter (fun pair -> (fst pair) > xMark && (snd pair) < yMark)

    { Partitions = [| (Q1, q1); (Q2, q2); (Q3, q3); (Q4, q4) |]
      Marks = (xMark, yMark) }

type QuadrantMaxResult =
    { quadrant: Quadrant
      area: int64
      position: (int64 * int64) }

let getQuadrantMax (splits: Splits) =
    let xMark, yMark = splits.Marks

    splits.Partitions
    |> Array.map (fun partition ->
        let quad = fst partition
        let pairs = snd partition

        let maxAreaWithPair =
            pairs
            |> Array.map (fun pair ->
                let area =
                    match quad with
                    | Quadrant.Q1 -> ((fst pair) - xMark) * ((snd pair) - yMark)
                    | Quadrant.Q2 -> (xMark - (fst pair)) * ((snd pair) - yMark)
                    | Quadrant.Q3 -> (xMark - (fst pair)) * (yMark - (snd pair))
                    | Quadrant.Q4 -> ((fst pair) - xMark) * (yMark - (snd pair))

                area, pair)
            |> Array.maxBy fst

        { quadrant = quad
          area = fst maxAreaWithPair
          position = snd maxAreaWithPair })

let getTotalMax (quadMax: QuadrantMaxResult array) =
    let q1 = quadMax |> Array.filter (fun a -> a.quadrant = Quadrant.Q1) |> Array.head
    let q2 = quadMax |> Array.filter (fun a -> a.quadrant = Quadrant.Q2) |> Array.head
    let q3 = quadMax |> Array.filter (fun a -> a.quadrant = Quadrant.Q3) |> Array.head
    let q4 = quadMax |> Array.filter (fun a -> a.quadrant = Quadrant.Q4) |> Array.head

    //Console.WriteLine(q1)
    //Console.WriteLine(q2)
    //Console.WriteLine(q3)
    //Console.WriteLine(q4)

    let a =
        ((fst q1.position) - (fst q3.position) + 1L)
        * ((snd q1.position) - (snd q3.position) + 1L)

    let b =
        ((fst q4.position) - (fst q2.position) + 1L)
        * ((snd q2.position) - (snd q4.position) + 1L)

    Math.Max(a, b)

type Problem =
    static member displaySolution problemInputs =
        problemInputs
        |> Array.iter (fun problemInput ->
            Console.WriteLine problemInput.Label
            Console.WriteLine("==============")
            let solution = getGrid >> splitQuadrant >> getQuadrantMax >> getTotalMax
            Console.WriteLine(solution problemInput.Path)
            //Console.WriteLine(problemInput.Path |> getGrid |> bruteForce)

            ())

    interface IProblem with
        static member displaySolution problemInputs = Problem.displaySolution problemInputs
