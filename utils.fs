module utils

[<Struct>]
type ProblemInput = { Label: string; Path: string }

#nowarn "3535"

type IProblem =
    static abstract member displaySolution: problemInput: ProblemInput array -> unit
