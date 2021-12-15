open System.IO

type Rules = Map<(char * char), char>

type PairsMap = Map<(char * char), int64>
type CountsMap = Map<char, int64>
type Polymer = CountsMap * PairsMap

let lines = File.ReadLines("input")

let startPolymerList = Seq.head lines |> Seq.toList

let startCounts = startPolymerList |> List.countBy id |> Map.ofList |> Map.map (fun _ count -> int64(count))
let startPairs = startPolymerList |> List.pairwise |> List.countBy id |> Map.ofList |> Map.map (fun _ count -> int64(count))

let startPolymer: Polymer = startCounts , startPairs

let rules = 
    lines
    |> Seq.skip 2
    |> Seq.map (fun str -> str.Split(" -> "))
    |> Seq.map (fun arr -> ((arr.[0].[0], arr.[0].[1]), Seq.head arr.[1]))
    |> Map.ofSeq

let incrementCounts (key) (amount: int64) (counts: Map<'a, int64>) : Map<'a, int64> =
    counts
    |> Map.change key (fun result -> 
        match result with
        | Some(count) -> Some(count+amount)
        | None -> Some(amount)
    )

let substitute ((counts, pairs): Polymer) (rules: Rules): Polymer= 
    pairs
    |> Map.fold (fun (icounts, ipairs) (left, right) count -> 
        let lookupResult = Map.tryFind (left, right) rules
        match lookupResult with 
        | Some(newChar) -> 
            (incrementCounts newChar count icounts), 
            (ipairs |> incrementCounts (left, newChar) count |> incrementCounts (newChar, right) count)
        | None -> icounts, ipairs |> incrementCounts (left, right) count
    ) (counts, Map.empty)
    
let getResult times = 
    let (counts, _) =
        seq {1..times}
        |> Seq.fold (fun polymer _ -> substitute polymer rules) startPolymer

    let sorted = counts |> Map.toArray |> Array.sortBy (fun (_, count) -> count)
    let _, mcount = sorted |> Array.last
    let _, lcount = sorted |> Array.head
    mcount - lcount

printfn "%A" (getResult 10)
printfn "%A" (getResult 40)

