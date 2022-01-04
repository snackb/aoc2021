
type Space = Empty | Amph of int

/// 0 1 x 2 x 3 x 4 x 5 6
///     7   8   9   10
///     11  12  13  14
///     15  16  17  18
///     19  20  21  22

let connectionsMap = Map [
    (0, [(1,1)])
    (1, [(0,1); (7,2); (2,2)])
    (2, [(1,2); (7,2); (3,2); (8,2)])
    (3, [(2,2); (8,2); (9,2); (4,2)])
    (4, [(3,2); (9,2); (10,2); (5,2)])
    (5, [(6,1); (10,2); (4,2)])
    (6, [(5,1)])
    (7, [(1,2); (2,2); (11,1)])
    (8, [(2,2); (3,2); (12,1)])
    (9, [(3,2); (4,2); (13,1)])
    (10, [(4,2); (5,2); (14,1)])
    (11, [(7,1)])
    (12, [(8,1)])
    (13, [(9,1)])
    (14, [(10,1)])
]

let destMap = Map [
    (1, [7; 11])
    (10, [8; 12])
    (100, [9; 13])
    (1000, [10; 14])
]
let getMap values =
    let mutable output = Array.create 15 Empty
    for (i, value) in Seq.indexed values do
        output[7 + i] <- Amph value
    output

let aim = getMap [1;10;100;1000;1;10;100;1000]

let lastBothOrNeither (state: Space array) first last =
    (state[first] <> Empty && state[last] = Empty) |> not

let notBlockingRoom state = 
    lastBothOrNeither state 7 11 &&
    lastBothOrNeither state 8 12 &&
    lastBothOrNeither state 9 13 &&
    lastBothOrNeither state 10 14

let onlyCorrectInRoom (state: Space array) origcost =
    destMap[origcost] 
    |> Seq.forall (fun roomSpace -> 
        state[roomSpace] = Empty ||
        state[roomSpace] = Amph origcost)

let onlyMoveToDest (state: Space array) orig move origcost = 
    if orig < 7 || move > 6 then List.contains move destMap[origcost] && onlyCorrectInRoom state origcost else true

let valid (state: Space array) = 
    destMap
    |> Map.exists (fun k v -> 
        v |> Seq.exists (fun pos -> state[pos] <> Amph k)
    ) |> not

let isLegal state orig move origcost =
    notBlockingRoom state &&
    onlyMoveToDest state orig move origcost

let getMoves (cost: int) (state: Space array): ((Space array) * int) seq =
    if valid state then Seq.empty else
    if state = aim then printfn "Yo!"

    let rec loop orig amphcost sumcost travelled = [
        let cur = List.head travelled
        let options = 
            connectionsMap[cur] 
            |> List.filter (fun (mv,_)->List.contains mv travelled |> not)
            |> List.filter (fun (mv,_)-> state[mv] = Empty)
        for (move, mult) in options do
            let newState = 
                state
                |> Array.updateAt orig Empty
                |> Array.updateAt move (Amph amphcost)
            let movecost = sumcost + mult * amphcost
            if isLegal newState orig move amphcost then yield (newState, movecost)
            yield! loop orig amphcost movecost (move :: travelled)
    ]

    let processSpace ((index, space): int * Space): ((Space array) * int) seq  =
        match space with
        | Empty -> []
        | Amph amphCost -> loop index amphCost cost [index]

    state |> Array.indexed
    |> Seq.collect processSpace
    |> Seq.groupBy (fun (move, _) -> move) |> Seq.map (fun (_,r)->r)
    |> Seq.map (Seq.minBy (fun (_,r)-> r))



let getCosts init =
    let rec loop (statesMap: Map<Space array, int>) =
        printfn "%A" (Map.count statesMap)
        let result = 
            statesMap
            |> Seq.collect (fun kvp -> getMoves kvp.Value kvp.Key)
            |> Seq.groupBy (fun (move, _) -> move) |> Seq.map (fun (_,r)->r)
            |> Seq.map (Seq.minBy (fun (_,r)-> r))
            |> Seq.fold (fun map (state, newcost) -> 
                let cur = Map.tryFind state map
                match cur with 
                | Some(oldcost) -> if newcost < oldcost then Map.add state newcost map else map
                | None -> Map.add state newcost map
                ) statesMap
        if result = statesMap then result else loop result
    loop (Map [(init,0)])

let state = getMap [1000;10;100;1;100;1;1000;10]

let emptyCorridor (state: Space array): bool = 
    seq {0 .. 6}
    |> Seq.forall (fun x -> state[x] = Empty)

let tst = [|Empty; Empty; Empty; Empty; Empty; Amph 1; Empty;
                    Empty;   Amph 10; Amph 100; Amph 1000;
                    Amph 1;  Amph 10; Amph 100; Amph 1000|]
let attempt = getMoves 12513 tst
printfn "%A" attempt

let result = 
    getCosts state
    |> Map.filter (fun k _-> emptyCorridor k)

printfn "%A" (Map.tryFind aim result)
id ()