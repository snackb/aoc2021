open System.IO

type Candidates = Map<char, char array>

let fullCandidates:Candidates = 
    Map.ofArray [|
        ('a', [|'a';'b';'c';'d';'e';'f';'g';|])
        ('b', [|'a';'b';'c';'d';'e';'f';'g';|])
        ('c', [|'a';'b';'c';'d';'e';'f';'g';|])
        ('d', [|'a';'b';'c';'d';'e';'f';'g';|])
        ('e', [|'a';'b';'c';'d';'e';'f';'g';|])
        ('f', [|'a';'b';'c';'d';'e';'f';'g';|])
        ('g', [|'a';'b';'c';'d';'e';'f';'g';|])
    |]

let lines = File.ReadLines("input")

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

let applyKnown (scrambled:string array) (num:int, segments:char list) (candidates:Candidates):Candidates =
    scrambled
    |> Array.filter (fun x -> x.Length = num)

let decodeMapping (input:string seq):Map<char, char array> = 
    fullCandidates
    |> 

/// 
/// restrictcandidates candidates unique =
///     blah
/// 
/// uniques
/// |> fold (restrictcandidates: candidates -> unique -> candidates) fullcandidates
/// |> apply segmap
/// |> decode segments
/// |> join, parse, sum
/// 