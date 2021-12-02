open System.IO
open System

let lines = File.ReadAllLines("input")

type Direction = Up | Down | Forward
type Instruction = Direction * int

let parseInstruction (str:string) = 
    let split = str.Split(' ')
    let direction = 
        match split[0] with
        | "forward" -> Forward
        | "up" -> Up
        | "down" -> Down
        | _ -> Down // silence warnings lol
    Instruction(direction, int(split[1]))

let processMovement (depth, distance) ((direction, speed):Instruction)  =
    match direction with 
    | Forward -> (depth, distance + speed)
    | Down -> (depth + speed, distance)
    | Up -> (depth - speed, distance)


let instructions = 
    lines
    |> Seq.map parseInstruction


let depth, distance = instructions |> Seq.fold processMovement (0, 0) 

printfn "Part one: %d" (depth * distance)

let processAim (depth, distance, aim) ((direction, speed):Instruction)  =
    match direction with 
    | Forward -> (depth + aim * speed, distance + speed, aim)
    | Down -> (depth, distance, aim + speed )
    | Up -> (depth, distance, aim - speed)

let depth2, distance2, _ = instructions |> Seq.fold processAim (0, 0, 0)

printfn "Part two: %d" <| depth2 * distance2