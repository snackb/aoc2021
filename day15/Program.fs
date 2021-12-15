open System.IO
open FSharpx.Collections

type Coord = int*int
type Graph = Map<Coord, int>
type Dist = Map<Coord, int>
type Prev = Map<Coord, Coord>
type Unvisited = Set<Coord>
type Visited = Set<Coord>

let lines = File.ReadLines("input")

let grid: Graph = 
    lines
    |> Seq.map (Seq.map (fun chr -> int(chr.ToString())))
    |> Seq.indexed 
    |> Seq.map (fun (x, values) -> 
        Seq.indexed values 
        |> Seq.map (fun (y, value) -> ((x,y), value)) )
    |> Seq.collect id
    |> Map.ofSeq

let getNeighbours (grid: Graph) ((thisx, thisy):Coord): Set<Coord> = 
    [(0,1);(0,-1);(1,0);(-1,0)]
    |> Seq.map (fun (xdiff, ydiff) -> (thisx+xdiff, thisy+ydiff))
    |> Seq.filter (fun coord -> Map.containsKey coord grid)
    |> Set.ofSeq

let processNode (grid: Graph) (cur: Coord) (prev: Prev, dist: Dist) (node: Coord): (Prev * Dist) =
    let thisDist = Map.find node dist
    let curDist = Map.find cur dist
    let thisCost = Map.find node grid
    let newDist = curDist + thisCost
    if newDist < thisDist then
        (
            Map.add node cur prev,
            Map.add node newDist dist
        )
    else (prev, dist)

let rec getPathBetween (path: Coord list) (start: Coord) (prev: Prev): Coord list =
    let prevNode = Map.find path.Head prev
    if prevNode = start then path
    else getPathBetween (prevNode :: path) start prev

let getMinPath (grid: Graph) (start: Coord) (dest: Coord): List<Coord> =
    let rec pathRec (prev: Prev) (dist: Dist) (unvi: Unvisited) (visited: Unvisited): Prev * Dist =
        let cur = (unvi |> Seq.minBy (fun coord -> Map.find coord dist))
        let nvisi = Set.add cur visited
        let neighbours = getNeighbours grid cur
        let nunvi = Set.remove cur unvi |> Set.union (Set.difference neighbours nvisi)
        let nprev, ndist =
            neighbours |> Set.intersect nunvi
            |> Seq.fold (processNode grid cur) (prev, dist)
        if (Map.tryFind dest nprev).IsSome then
            nprev, ndist
        else
            pathRec nprev ndist nunvi nvisi
    let dist: Dist = grid|> Map.map (fun _ _ -> 9999)|> Map.add start 0
    let prev: Prev = Map.empty
    let unvi: Unvisited = Set.empty |> Set.add start
    let endprev, enddist = pathRec prev dist unvi Set.empty
    getPathBetween [dest] start endprev

let start = (0, 0)
let width = Seq.length (Seq.head lines)
let height = Seq.length lines
let dest = (width - 1, height - 1)

// let path = getMinPath grid start dest
// let pathSum = path |> List.map (fun coord -> Map.find coord grid) |> Seq.sum
// printfn "%A" path
// printfn "%A" pathSum

let mapvalue offset orig = if offset + orig > 9 then ((offset + orig) % 9) else offset + orig

let newGrid =
    let mults = Seq.allPairs (seq { 0 .. 4}) (seq { 0 .. 4})
    grid
    |> Seq.collect (fun kvp -> 
        let (x, y) = kvp.Key
        mults
        |> Seq.map (fun (xm, ym) -> 
            (xm * width + x, ym * height + y), mapvalue (xm + ym) kvp.Value )
    )
    |> Map.ofSeq

printfn "%A" (Map.count grid)
printfn "%A" (Map.count newGrid) 
    
let newDest = ((width * 5) - 1), ((height * 5) - 1)
let newPath = getMinPath newGrid start newDest
let newPathCosts = newPath |> List.map (fun coord -> Map.find coord newGrid)
let newPathSum = newPathCosts |> Seq.sum
printfn "%A" newPath
printfn "%A" newPathSum


