open System.IO
open System

type Number = Literal of int | Pair of Number * Number

type ExplosionResult = Option<Number * (Option<int> * Option<int>)>

let lines = File.ReadLines("input")

let oneOf (first: 'T -> Result<'U, 'E>) (second: 'T -> Result<'U, 'E>) (input: Result<'T, 'E>): Result<'U, 'E> = 
    let firstResult = Result.bind first input
    match firstResult with
    | Ok(_) -> firstResult
    | Error(_) -> Result.bind second input

let expect chr (str: string): Result<string, string> =
    if str[0] = chr then Ok(str[1..])
    else Error str 

let parseLiteral (str: string): Result<Number * string, string> = 
    if Char.IsDigit str[0] then Ok(Literal(int str[0..0]), str[1..])
    else Error str

let rec parsePair str: Result<Number * string, string> = 
    expect '[' str
    |> oneOf parseLiteral parsePair
    |> Result.bind (fun (left, rem) -> 
        expect ',' rem
        |> oneOf parseLiteral parsePair
        |> Result.bind (fun (right, rem) -> 
            expect ']' rem
            |> Result.map (fun rem -> 
                (Pair (left, right)), rem
            )
        )
    )

let rec addToLeftMost (amount: int) num: Number =
    match num with 
    | Pair (l, r) -> Pair ((addToLeftMost amount l), r)
    | Literal i -> Literal (i + amount)

let rec addToRightMost (amount: int) num: Number =
    match num with 
    | Pair (l, r) -> Pair (l, addToRightMost amount r)
    | Literal i -> Literal (i + amount)



let rec explodeRec (count: int) (number: Number) : ExplosionResult =
    match number, count > 3 with
    | Literal i, _ -> None
    | Pair (Literal l, Literal r), true -> 
        Some(Literal 0, (Some l, Some r))
    | Pair (l, r), _ -> 
        explodeRec (count+1) l |> Option.map (fun (num, expl) -> 
            match expl with
            | (Some le, Some re) -> (Pair (num, (addToLeftMost re r))), (Some le, None)
            | (Some le, None) -> Pair (num, r), (Some le, None)
            | (None, Some re) -> Pair (num, addToLeftMost re r), (None, None)
            | (None, None) -> Pair (num, r), (None, None)
        ) 
        |> Option.orElse (
            explodeRec (count+1) r
            |> Option.map (fun (num, expl) -> 
            match expl with 
            | (Some le, Some re) -> (Pair ((addToRightMost le l), num)), (None, Some re)
            | (Some le, None) -> Pair (addToRightMost le l , num), (None, None)
            | (None, Some re) -> Pair (l, num), (None, Some re)
            | (None, None) -> Pair (l, num), (None, None)
            )
        )

let explode: Number -> Option<Number> = 
    explodeRec 0
    >> Option.map (fun (num, _) -> num)


let rec split (number: Number): Option<Number> =
    match number with 
    | Literal i when i > 9 -> Some(Pair ((Literal (i/2)), (Literal (i/2 + i%2))))
    | Pair (l, r) -> 
        (split l |>  Option.map (fun l -> Pair (l, r)))
        |> Option.orElse ((split r) |> Option.map (fun r -> (Pair (l, r))))
    | _ -> None

let rec add (l: Number) (r: Number): Number =
    let pair = Pair (l, r)

    let rec addRec num =
        match explode num |> Option.orElse (split num) with
        | Some(num) -> addRec num
        | None -> num

    addRec pair


let rec magnitude (number: Number): int =
    match number with
    | Literal i -> i
    | Pair (l, r) -> 3 * (magnitude l) + 2 * (magnitude r)
        
let nums = lines |> Seq.map parsePair |> Seq.map (fun res -> match res with | Ok(num, _) -> num)

let num = nums |> Seq.reduce add

printfn "%A" (magnitude num)

let largestPair l r =
    max (magnitude (add l r)) (magnitude (add r l))

let maxMagnitude =
    (nums |> Seq.indexed)
    |> Seq.allPairs (nums |> Seq.indexed)
    |> Seq.filter (fun ((i, l), (j, r)) -> i <> j)
    |> Seq.map (fun ((_, l), (_, r)) -> largestPair l r)
    |> Seq.max

printfn "%A" maxMagnitude