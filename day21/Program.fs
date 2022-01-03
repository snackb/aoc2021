open System.IO
open System
open System.Collections.Generic

type Player = int * int // score * pos

//let split (delim: String) (s: String) = s.Split(delim)

//let txt = File.ReadAllText("input")

let detd100 = 
    Seq.initInfinite (fun i -> (i % 100) + 1)

let takeTurn (die: int seq) ((score, pos): Player): Player * int seq =
    let roll = Seq.take 3 die |> Seq.sum
    let newpos = (roll + pos) % 10
    let newScore = if newpos = 0 then 10 else newpos
    ((newScore + score, newpos), Seq.skip 3 die)

let isWinner (score, _) = score >= 1000

let hasWinner: Player seq -> bool =
    Seq.exists isWinner

let rec playTurns (players: Player list) (die: int seq) (rolled: int): Player list * int = 
    let newplayers, newdie = 
        players |> List.mapFold takeTurn die
    if hasWinner newplayers then newplayers, rolled + (3 * (1 + List.findIndex isWinner newplayers))
    else playTurns newplayers newdie ((3 * (List.length newplayers)) + rolled)

let result = playTurns [(0, 4); (0, 8)] detd100 0

printfn "%A" result

let newResult = playTurns [(0, 6); (0, 9)] detd100 0

printfn "%A" newResult


let processPlayer ((score, pos): Player) (roll: int) : Player = 
    let newpos = (roll + pos) % 10
    let newScore = if newpos = 0 then 10 else newpos
    (score+newScore, newpos)

let dice: (int64 * int64) list = 
    [(3,1);(4,3);(5,6);(6,7);(7,6);(8,3);(9,1)]

let wrapTo10 (n: int64): int64 =
    if n % int64(10) = 0 then 10 else n % int64(10)

let rec playTurn (p1: int64) (p2: int64) (s1:int64) (s2:int64): int64 * int64 =
    if s2 >= 21 then 0, 1
    else
    dice
    |> Seq.fold (fun (wins1, wins2) (move, n) -> 
        let newpos = wrapTo10 (p1 + move)
        let (w2, w1) = playTurn p2 newpos s2 (s1 + newpos)
        (wins1 + (n*w1), wins2 + (n*w2))) (0, 0)

printfn "%A" (playTurn 6 9 0 0)