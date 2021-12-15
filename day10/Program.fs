open System.IO

let lines = File.ReadLines("input")

let closes = [')'; '}'; ']'; '>']
let isClose = (fun x -> List.contains x closes)
let opens = ['('; '{'; '['; '<']
let isOpen = (fun x -> List.contains x opens)

let isPair opener closer = (List.findIndex ((=)opener) opens) = List.findIndex ((=)closer) closes

let rec recurseFindIncorrect (inlist:char list) (stack:char list):Option<char> =
    match inlist with 
    | [] -> None
    | cur :: rem -> 
        if isOpen cur then
            recurseFindIncorrect rem (cur::stack)
        else 
            match stack with 
                | [] -> None
                | top :: bot -> 
                    if isPair top cur then
                        recurseFindIncorrect rem bot
                    else 
                        Some cur

    

let findFirstIncorrectChar (input:string):Option<char> =
    recurseFindIncorrect 
        (input |> Seq.toList)
        []

let scoreMap = 
    [(')', 3); (']', 57); ('}',1197);('>',25137)]
    |> Map.ofList

let score = 
    lines
    |> Seq.map (fun x -> x.Trim())
    |> Seq.map findFirstIncorrectChar
    |> Seq.filter Option.isSome 
    |> Seq.map Option.get
    |> Seq.map (fun x -> Map.find x scoreMap)
    |> Seq.sum

printfn "%A" score

let rec recurseFindUnmatched (inlist:char list) (stack:char list):List<char> =
    match inlist with 
    | [] -> stack
    | cur :: rem -> 
        if isOpen cur then
            recurseFindUnmatched rem (cur::stack)
        else 
            let _ :: bot = stack
            recurseFindUnmatched rem bot

let findUnmatched (input:string):List<char> =
    recurseFindUnmatched 
        (input |> Seq.toList)
        []

let scoreMap2 = [(')', 1); (']', 2); ('}',3);('>',4)] |> Map.ofList

let scoreList (inlist: char list) =
    inlist
    |> List.map (fun x -> List.find (isPair x) closes)
    |> List.fold (fun acc x -> (acc * int64(5)) + int64((Map.find x scoreMap2))) (int64(0))

let scores =
    lines
    |> Seq.filter (findFirstIncorrectChar >> Option.isNone)
    |> Seq.map (findUnmatched >> scoreList) 
    |> Seq.sort

let len = Seq.length scores 
let half = (len / 2) 
let score2 = 
    scores 
    |> Array.ofSeq 
    |> Array.get 
    <| ((Seq.length scores) /2)
printfn "%A" scores
printfn "%A" score2
