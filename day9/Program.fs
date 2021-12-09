open System.IO

type Coord = int*int
type FloorMap = Map<Coord, int>

let lines = File.ReadLines("input")

let floorMap:FloorMap =
    lines
    |> Seq.map (fun x -> x.ToCharArray())
    |> Seq.indexed
    |> Seq.map (fun (xindex, row) -> (xindex, Seq.indexed row))
    |> Seq.collect (fun (xindex, row) -> Seq.map (fun (yindex, height) -> ((xindex, yindex), int(height.ToString()))) row )
    |> Map.ofSeq

printfn "%A" floorMap

let lowerThan (height:int) (otherHeight:Option<int>):bool =
    match otherHeight with
    | Some(other) -> height < other
    | None -> true

let isLow (floor:FloorMap) ((thisx, thisy):Coord):bool =
    let thisHeight = Map.find (thisx, thisy) floor
    [(0,1);(0,-1);(1,0);(-1,0)]
    |> Seq.map (fun (xdiff, ydiff) -> Map.tryFind (thisx + xdiff, thisy+ydiff) floor)
    |> Seq.map (lowerThan thisHeight)
    |> Seq.forall id

let lowPoints = 
    floorMap
    |> Map.filter (fun k v -> isLow floorMap k)

printfn "%A" (lowPoints 
    |> Seq.map (fun x -> x.Value + 1)
    |> Seq.sum)

type Basin = Set<Coord>

let getNeighbours ((thisx, thisy):Coord): Set<Coord> = 
    [(0,1);(0,-1);(1,0);(-1,0)]
    |> Seq.map (fun (xdiff, ydiff) -> (thisx+xdiff, thisy+ydiff))
    |> Seq.filter (fun coord -> Map.containsKey coord floorMap)
    |> Seq.filter (fun coord -> not ((Map.find coord floorMap) = 9))
    |> Set.ofSeq

let rec extendBasin (floorMap:FloorMap) (currentBasin: Basin) : Basin =
    let frontier = 
        currentBasin // for each of the coords in the current basin
        |> Seq.map (getNeighbours >> (fun ne -> Set.difference ne currentBasin)) // find the neighbors not already in it
        |> Seq.reduce Set.union 
    if Set.isEmpty frontier then // surrounded by 9's, basin complete base case
        currentBasin
    else // find the next frontier
        extendBasin floorMap (Set.union currentBasin frontier)

let getBasin (lowpoint:Coord):Basin =
    extendBasin floorMap (Set.singleton lowpoint)

let start = System.DateTimeOffset.Now

let basins =
    lowPoints
    |> Seq.map ((fun kvp -> kvp.Key) >> getBasin >> Set.count)
    |> Seq.sort
    |> Seq.rev
    |> Seq.take 3
    |> Seq.reduce (*)

let elapsed = System.DateTimeOffset.Now - start

printfn "%A" basins
printfn "%A" elapsed