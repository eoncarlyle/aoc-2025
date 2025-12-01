module day1

open System.IO
open System.Text.RegularExpressions
open System


[<Struct>]
type Direction =
    | Left
    | Right

[<Struct>]
type Vector = { Direction: Direction; Size: int }

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

let (|Integer|_|) (str: string) =
    match System.Int32.TryParse str with
    | true, i -> Some i
    | false, _ -> None

let getDirection (line: string) =
    if line.Equals("L") then Direction.Left else Direction.Right

let parse (row: string) =
    match row with
    | ParseRegex "(L|R)(\d+)" [ direction; Integer size ] ->
        { Direction = getDirection direction
          Size = size }
    | _ -> failwith "Invalid input"


[<Struct>]
type State = { Position: int; ZeroCount: int }

let evaluatePart1 (state: State) (adjustment: Vector) : State =
    let sign = if adjustment.Direction = Direction.Left then -1 else 1
    let rawPosition = state.Position + sign * adjustment.Size

    let position =
        if rawPosition > 100 then
            rawPosition % 100
        elif rawPosition = 100 then
            0
        elif rawPosition < 0 then
            let inner = 100 + (rawPosition % 100)
            if inner = 100 then 0 else inner
        else
            rawPosition

    let zeroIncrement = if position = 0 then 1 else 0

    { Position = position
      ZeroCount = state.ZeroCount + zeroIncrement }

let evaluatePart2 (state: State) (adjustment: Vector) : State =
    let sign = if adjustment.Direction = Direction.Left then -1 else 1
    let rawPosition = state.Position + sign * adjustment.Size

    let position =
        if rawPosition > 100 then
            rawPosition % 100
        elif rawPosition = 100 then
            0
        elif rawPosition < 0 then
            let inner = 100 + (rawPosition % 100)
            if inner = 100 then 0 else inner
        else
            rawPosition

    let finalWheelFrame = // The problem is that position 86 to 0 is a frame change of 0
        match rawPosition with
        | rawPosition when rawPosition < 0 -> (-1 * rawPosition + 100) / 100
        | rawPosition when rawPosition = 0 -> 1
        | _ -> rawPosition / 100

    { Position = position
      ZeroCount = state.ZeroCount + finalWheelFrame }

let paths = [| ("Test", "./day1/test.txt"); ("Prod", "./day1/prod.txt") |]

let solution () =
    paths
    |> Array.iter (fun a ->
        Console.WriteLine(fst a)
        Console.WriteLine("==============")

        let lines = File.ReadAllLines <| snd a

        Console.WriteLine "Part 1"

        let part1 =
            Array.fold evaluatePart1 { Position = 50; ZeroCount = 0 }
            <| Array.map parse lines

        Console.WriteLine part1

        Console.WriteLine "Part 2"

        let part2 =
            Array.fold evaluatePart2 { Position = 50; ZeroCount = 0 }
            <| Array.map parse lines

        Console.WriteLine part2

        ())
