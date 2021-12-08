open System.IO

let line = File.ReadLines("testinput") |> Seq.head
let crabs:int seq = 
    line.Split(",") |> Seq.map int

let min = Seq.min crabs
let max = Seq.max crabs

let fuel (crabs:int seq)  (pos:int) = 
    crabs 
    |> Seq.map (fun crab -> abs(crab-pos))
    |> Seq.sum

let minFuel = 
    seq {min .. max}
    |> Seq.map (fuel crabs)
    |> Seq.min

printfn "%A" minFuel

let triangleFuel (pos:int) (crab:int) =
    crab-pos 
    |> abs
    |> (fun n -> n *(n+1) / 2)

let morefuel (crabs:int seq)  (pos:int) = 
    crabs 
    |> Seq.map (triangleFuel pos)
    |> Seq.sum

let newMin = 
    seq {min .. max}
    |> Seq.map (morefuel crabs)
    |> Seq.min

printfn "%A" newMin