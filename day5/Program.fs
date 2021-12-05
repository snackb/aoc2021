open System.IO

type Coord = int * int
type Line = Coord * Coord

let lines = File.ReadAllLines("input")

let paths: Line array = 
    lines
    |> Seq.map (fun x -> 
        x.Split("->") |> 
        Array.map ((fun y -> y.Split(',') |> Array.map int) >> fun z -> (z.[0], z.[1])))
        |> Seq.map (fun coords -> (coords.[0], coords.[1]))
        |> Seq.toArray

let mutable intersections = 0

let straightPaths = 
    paths
    |> Array.filter (fun ((startx, starty), (endx, endy)) -> (startx = endx) || (starty = endy))

let bothwaysSequence linestart lineend = 
    if linestart < lineend then
        seq {linestart..lineend}
    else 
        seq {lineend..linestart}

let straightLinePoints (line:Line):Coord array = 
    let ((startx, starty), (endx, endy)) = line
    if startx = endx then 
        bothwaysSequence starty endy
        |> Seq.map (fun ycoord -> (startx, ycoord))
        |> Seq.toArray
    else
        bothwaysSequence startx endx
        |> Seq.map (fun xcoord -> (xcoord, starty))
        |> Seq.toArray

let diagonalLinepoints (((startx, starty), (endx, endy)):Line):Coord array = 
    if (startx - endx) = (starty - endy) then 
        let xs = bothwaysSequence startx endx
        let ys = bothwaysSequence starty endy
        Seq.zip xs ys
        |> Seq.toArray
    else 
        let xs = bothwaysSequence startx endx
        let ys = (bothwaysSequence starty endy) |> Seq.rev
        Seq.zip xs ys
        |> Seq.toArray

let linePoints (line:Line):Coord array = 
    let ((startx, starty), (endx, endy)) = line
    if startx = endx then 
        bothwaysSequence starty endy
        |> Seq.map (fun ycoord -> (startx, ycoord))
        |> Seq.toArray
    else if starty = endy then
        bothwaysSequence startx endx
        |> Seq.map (fun xcoord -> (xcoord, starty))
        |> Seq.toArray
    else 
        diagonalLinepoints line
                
let straightCoords =
    straightPaths
    |> Array.collect linePoints
    |> Array.groupBy id
    |> Array.filter (fun (_, arr) -> arr.Length > 1)

printfn "%A" straightCoords.Length



let coords = 
    paths
    |> Array.collect linePoints
    |> Array.groupBy id
    |> Array.filter (fun (_, arr) -> arr.Length > 1)
    |> Array.map (fun (_, arr) -> arr)

printfn "%A" coords.Length