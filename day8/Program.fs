open System.IO

let lines = File.ReadLines("input")



let sortStrings strings = strings |> Array.map (Array.ofSeq >> Array.sort >> System.String) 

let inputSegments = 
    lines 
    |> Seq.map (fun x -> (x.Split "|"))
    |> Seq.map (Array.map (fun x -> x.Split " "))
    |> Seq.map (Array.map (Array.filter (fun str -> str.Length > 1)))
    |> Seq.map (Array.map sortStrings)

let counts = 
    inputSegments
    |> Seq.map (fun x -> x.[1])
    |> Seq.collect id
    |> Seq.filter (fun x -> 
        match x.Length with
        | 2 | 3 | 4 | 7 -> true
        | _ -> false)
    |> Seq.length

printfn "%A" counts // Part one

let segmap = Map.ofArray [|
    ("abcefg", "0");
    ("cf", "1");
    ("acdeg", "2");
    ("acdfg", "3");
    ("bcdf", "4");
    ("abdfg", "5");
    ("abdefg", "6");
    ("acf", "7");
    ("abcdefg", "8");
    ("abcdfg", "9");
 |]

type CandidatesMap = Map<char, Set<char>>

let fullCandidatesMap:CandidatesMap = 
    seq {'a' .. 'g' }
    |> Seq.map (fun x -> (x, seq {'a'..'g'} |> Set.ofSeq))
    |> Map.ofSeq

let choose (target:string) (candidates:CandidatesMap) (segment:string):CandidatesMap =
    let segmentSet = segment |> Set.ofSeq
    candidates
    |> Map.map ( fun k v -> 
        if target.Contains k then 
            (Set.intersect v segmentSet)
        else 
            v
    ) 

let rec decodeMapping (segments: Set<string>) (candidates:CandidatesMap) (choices:Map<string, string>) (input:string list):Option<Map<string, string>>= 
    match input with
    | [] -> Some(choices)
    | head :: tail -> (
        segments
        |> Seq.filter (fun segment -> segment.Length = head.Length)
        |> Seq.map ( fun segment -> 
            let resultCandidates = choose head candidates segment
            if Map.forall (fun _ options -> not (Set.isEmpty options)) resultCandidates then
                decodeMapping (Set.remove segment segments) resultCandidates (Map.add head segment choices) tail 
            else None
        ) 
        |> Seq.tryFind Option.isSome
        |> Option.flatten
    )

let defaultDecode = List.ofArray >> decodeMapping (segmap.Keys |> Set.ofSeq) fullCandidatesMap Map.empty >> Option.get

let result = 
    inputSegments
    |> Seq.map (fun x -> (x.[0], x.[1]))
    |> Seq.map (fun (left, right) -> (defaultDecode left, right))
    |> Seq.map (fun (left, right) -> 
        Array.map (fun scrambled -> Map.find scrambled left) right
    )
    |> Seq.map (Array.map (fun unscrambled -> Map.find unscrambled segmap))
    |> Seq.map (String.concat "")
    |> Seq.map int
    |> Seq.sum

printfn "%A" result