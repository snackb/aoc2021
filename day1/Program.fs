open System.IO;
open System

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

let lines = File.ReadAllLines("input")
let depths = 
    lines |> Seq.map(fun str -> int(str))

let folder (count, prev) curr = 
    if curr > prev then (count+1, curr)
    else (count, curr)

let count, _ = 
    depths
    |> Seq.fold folder  (0, Int32.MaxValue)

printfn "%d" count

let windowsCount, _ = 
    depths
    |> Seq.windowed 3
    |> Seq.map Seq.sum
    |> Seq.fold folder (0, Int32.MaxValue)

printfn "%d" windowsCount