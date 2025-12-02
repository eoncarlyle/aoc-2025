# README.md

Day 1: No matter what check that prod works before doing test, got throttled as I made successive attempts

Day starter:

```fsharp
module dayN

open System

let problemInputs: ProblemInput array = [||]

type Problem() =
    static member displaySolution problemInputs = ()

    interface IProblem with
        static member displaySolution problemInputs = Problem.displaySolution problemInputs
```
