module day1

open System.IO
open System.Text.RegularExpressions


let lines = File.ReadAllLines "./day1/test.txt"

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

let evaluate (state: State) (adjustment: Vector) : State =
    let sign = if adjustment.Direction = Direction.Left then -1 else 1
    let rawPosition = state.Position + sign * adjustment.Size

    let position =
        if rawPosition > 100 then rawPosition % 100
        elif rawPosition = 100 then 0
        elif rawPosition < 0 then 100 + (rawPosition % 100)
        else rawPosition

    let zeroIncrement = if position = 0 then 1 else 0

    { Position = position
      ZeroCount = state.ZeroCount + zeroIncrement }


let part0 =
    Array.fold evaluate { Position = 50; ZeroCount = 0 } <| Array.map parse lines
