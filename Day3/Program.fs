open System.IO
open System

let lines = File.ReadAllLines("input")

let codes =
    lines
    |> Seq.map Seq.toArray

let getCounts (codes:seq<array<char>>) = 
    let mutable counts = Array.create (codes |> Seq.head).Length 0
    for code in codes do
        for i in 0..(code.Length-1) do
            match code.[i] with
            | '1' -> counts.[i] <- counts.[i] + 1
            | '0' -> counts.[i] <- counts.[i] - 1
    counts

let mutable counts = getCounts codes


let mutable gamma = 0
let mutable epsilon = 0

for i in 0..(counts.Length-1) do
    if counts.[counts.Length-(i+1)] > 0 then
        gamma <- gamma + int(2.0 ** float(i))
    else
        epsilon <- epsilon + int((2.0 ** float(i)))

printfn "%d, %d" gamma epsilon 
printfn "%d" (gamma * epsilon)

let oxycommon incounts =
    incounts
    |> Seq.map (fun x -> if x > -1 then '1' else '0')
    |> Seq.toArray

let co2common incounts =
    incounts
    |> Seq.map (fun x -> if x < 0 then '1' else '0')
    |> Seq.toArray

        
let codesarray = codes |> Seq.toArray

let rec findby (codes:array<array<char>>) (commonfn:seq<int>->array<char>) (i:int) = 
    let comm = (codes |> getCounts |> commonfn)
    if codes.Length <= 1 then 
        Seq.head codes 
    else 
        findby 
            (codes |> Seq.filter (fun x -> x.[i] = comm.[i]) |> Seq.toArray)
            commonfn
            (i+1)

    

let oxynumber = findby codesarray oxycommon 0
let co2number = findby codesarray co2common 0
let oxy = Convert.ToInt32(String.Join("", oxynumber), 2)
let co2 = Convert.ToInt32(String.Join("", co2number), 2)

printfn "%A" (oxy * co2)