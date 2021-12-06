open System.IO

type Fishes = int64 array

let addFishes (count:int64) (fish:int) (fishes:Fishes) =
    fishes 
    |> Array.updateAt fish (fishes.[fish] + count)

let line = File.ReadLines("input") |> Seq.head
let fishes:Fishes = 
    line.Split(",") |> Array.map int 
    |> Array.fold 
        (fun state fish -> addFishes 1 fish state) (Array.create 10 (int64 0))

let evolve (fishes:Fishes):Fishes = 
    fishes 
    |> Array.indexed 
    |> Array.fold (fun fishes (fish, count) -> 
        match fish with 
        | 0 -> fishes |> addFishes count 6 |> addFishes count 8
        | _ -> addFishes count (fish-1) fishes) (Array.create 10 (int64 0))
printfn "%A" fishes

let rec run fishes days = 
    match days with 
    | 0 -> fishes
    | _ -> run (evolve fishes) (days-1)

printfn "%A" (run fishes 80 |> Array.sum)
printfn "%A" (run fishes 256 |> Array.sum)

