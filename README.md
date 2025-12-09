# README.md

Day 8:
```fsharp
//The type 'HashSet<Point>' does not support the 'comparison' constraint. For example, it does not support the 'System.IComparable' interface (F# Compiler 1)
let a: Map<HashSet<Point>, int> = Map []

```

Day 6: I don't know why this didn't build

```fsharp
[<Struct>] //Can't be struct?
 type Instruction =
     | Argument of double
     | Operator of String
```

Day 4: `dotnet build && dotnet fsi --load:repl.fsx`

Day 3: REPL workflow: `dotnet fsi` in `bin/Debug/net8.0` then `#r "aoc-2025.dll"`

Day 1: No matter what check that prod works before doing test, got throttled as I made successive attempts

Day starter:

```fsharp
module dayN

open utils
open System

type Problem() =
    static member displaySolution problemInputs =
        problemInputs |> Array.iter (fun problemInput ->
            Console.WriteLine problemInput.Label
            Console.WriteLine("==============")
            ())

    interface IProblem with
        static member displaySolution problemInputs = Problem.displaySolution problemInputs
```
