module utils

[<Struct>]
type ProblemInput = { Label: string; Path: string }


let getProblemInputs dayLabel : ProblemInput array =
    [| { Label = "Personal Test"
         Path = $"./{dayLabel}/personal-test.txt" }
       { Label = "AOC Test"
         Path = $"./{dayLabel}/aoc-test.txt" }
       { Label = "Prod"
         Path = $"./{dayLabel}/prod.txt" } |]


#nowarn "3535"

type IProblem =
    static abstract member displaySolution: problemInput: ProblemInput array -> unit
