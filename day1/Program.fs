open System.IO
open System

let lines = File.ReadAllLines("input")
let depths = 
    lines |> Seq.map(fun str -> int(str))

let folder (count, prev) curr = 
    if curr > prev then (count+1, curr)
    else (count, curr)

let count = 
    depths
    |> Seq.windowed 2
    |> Seq.filter (fun x -> x[0] < x[1])
    |> Seq.length

printfn "Part one: %d" count

let windowsCount = 
    depths
    |> Seq.windowed 3
    |> Seq.map Seq.sum
    |> Seq.windowed 2
    |> Seq.filter (fun x -> x[0] < x[1])
    |> Seq.length

printfn "Part two: %d" windowsCount