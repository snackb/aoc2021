open System.IO

type CandidatesMap = Map<char, Set<char>>


let fullCandidatesMap:CandidatesMap = 
    seq {'a' .. 'g' }
    |> Seq.map (fun x -> (x, seq {'a'..'g'} |> Set.ofSeq))
    |> Map.ofSeq

let lines = File.ReadLines("testinput")

let rightsegments = 
    lines 
    |> Seq.map (fun x -> (x.Split "|").[1])
    |> Seq.map (fun x -> x.Split " ")
    |> Seq.collect id

let counts = 
    rightsegments
    |> Seq.filter (fun x -> 
        match x.Length with
        | 2 | 3 | 4 | 7 -> true
        | _ -> false)
    |> Seq.length

printfn "%A" counts

let leftsegments = 
    lines 
    |> Seq.map (fun x -> (x.Split "|").[0])
    |> Seq.map (fun x -> x.Split " ")


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

let choose (target:string) (candidates:CandidatesMap) (segment:string):CandidatesMap =
    let segmentSet = segment |> Set.ofSeq
    candidates
    |> Map.map ( fun k v -> 
        if target.Contains k then 
            (Set.intersect v segmentSet)
        else 
            v)

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

let sortStrings strings = strings |> Array.map (Array.ofSeq >> Array.sort >> System.String) |>  List.ofSeq

let testLine = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab".Split(" ") |> sortStrings
let test = 
    testLine
    |> decodeMapping (segmap.Keys |> Set.ofSeq) fullCandidatesMap Map.empty
    |> Option.get

printfn "%A" (Map.toList test)

let testNumerals = 
    "cdfeb fcadb cdfeb cdbaf".Split(" ") |> sortStrings
    |> List.map (fun scrambledDigit -> Map.find scrambledDigit test)
    |> List.map (fun unscrambledDigit -> Map.find unscrambledDigit segmap)

printfn "%A" testNumerals
/// 
/// uniques -> segmap
/// unique -> segment implies all char in unique -> char in segment
/// 