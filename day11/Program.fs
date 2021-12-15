open System.IO

type Coord = int*int
type OctoMap = Map<Coord, int>

let lines = File.ReadLines("testinput")

let grid = 
    lines
    |> Seq.map (Seq.map (fun chr -> int(chr.ToString())))
    |> Seq.indexed 
    |> Seq.map (fun (x, values) -> 
        Seq.indexed values 
        |> Seq.map (fun (y, value) -> ((x,y), value)) )
    |> Seq.collect id
    |> Map.ofSeq

let addCoords (x1,y1) (x2,y2) = x1+x2, y1+y2

let neighbours coord = [-1;0;1] |> Seq.allPairs [-1;0;1] |> Seq.filter ((<>) (0,0)) |> Seq.map (addCoords coord)

let incrementNeighbours (map: OctoMap) (coord: Coord): OctoMap =
    neighbours coord
    |> Seq.fold ( fun innerMap neighbour -> 
        Map.change neighbour (Option.map ((+) 1)) innerMap
    ) map

let rec processFlashes (flashed: Coord seq) map: int * OctoMap =
    let flashes = map |> Map.filter (fun coord _ -> not (Seq.contains coord flashed)) |> Map.filter (fun _ intensity -> intensity > 9)
    if flashes = Map.empty then
        let reset = flashed |> Seq.fold (fun innerMap coord -> Map.add coord 0 innerMap) map 
        (Seq.length flashed, reset)        
    else 
        flashes.Keys
        |> Seq.fold incrementNeighbours map
        |> processFlashes (Seq.append flashed flashes.Keys)
    

let processStep flashes map: int * OctoMap =
    map
    |> Map.map (fun _ intensity -> intensity+1) 
    |> processFlashes Seq.empty
    |> (fun (count, result) -> (count+flashes, result))


let result, _ = 
    seq {1 .. 100}
    |> Seq.fold (fun (acc, grid) _ -> processStep acc grid) (0, grid)

printfn "%A" result

let (result2, _) = 
    grid 
    |> Seq.unfold 
        (fun state -> 
            let count, result = processStep 0 state
            if count = result.Count then None
            else Some(count, result)
        )
    |> Seq.zip (Seq.initInfinite ((+) 1))
    |> Seq.last

printfn "%A" result2

    