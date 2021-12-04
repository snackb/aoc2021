open System.IO

type Board = int[][]

let lines = File.ReadAllLines("input")

let notEmpty string = System.String.IsNullOrEmpty string |> not

let header = Seq.head lines

let nums = header.Split [|','|] |> Array.map int


let parseBoard (input:string[]) : Board =
    input
    |> Array.map (fun x -> x.Split [|' '|] |> Array.filter notEmpty |> Array.map int32)

let boards = 
    Seq.tail lines 
    |> Seq.filter notEmpty
    |> Seq.chunkBySize 5
    |> Seq.map parseBoard

let markBoard (num:int) (board:Board):Board = 
    board
    |> Array.map (fun row -> 
        row |> Array.map (fun entry -> 
            if entry = num then -1 else entry
        )
    )

let markBoards (num:int) (boards:array<Board>) : array<Board> = 
    boards
    |> Array.map (markBoard num)

let isWinner (board:Board):bool = 
    let cols = Array.transpose board
    Array.append board cols
    |> Array.exists (fun x -> x = [| -1;-1;-1;-1;-1 |])

let checkWinners (boards:array<Board>):Option<Board> =
    boards
    |> Array.tryFind isWinner

let calculateSum (num:int) (board:Board) =
    board
    |> Array.collect id 
    |> Array.filter (fun x -> x > -1)
    |> (Array.sum >> (*) num)

let rec findWinner (nums:array<int>) boards = 
    let markedBoards = markBoards nums.[0] boards
    match checkWinners markedBoards with
        | Some(winner) -> calculateSum nums.[0] winner
        | None -> findWinner (Array.tail nums) markedBoards

printfn "%A" (findWinner nums (boards|> Seq.toArray))


let rec findLastWinner (nums:array<int>) (boards:array<Board>) = 
    let markedBoards = markBoards nums.[0] boards
    match markedBoards.Length with 
    | 1 -> 
        if isWinner markedBoards.[0] then calculateSum nums.[0] markedBoards.[0]
        else findLastWinner (Array.tail nums) markedBoards
    | _ -> 
        let loserBoards = markedBoards |> Array.filter (isWinner >> not)
        findLastWinner (Array.tail nums) loserBoards

printfn "%A" (findLastWinner nums (boards|> Seq.toArray))