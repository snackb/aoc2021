open System.IO
open System

type Coord = int * int * int
type Probe = Set<Coord>

let split (delim: String) (s: String) = s.Split(delim)

let txt = File.ReadAllText("input")
let probes: Probe seq = 
    split "\n\n" txt
    |> Array.map (split "\n" 
                >> Seq.skip 1 
                >> Seq.map (split ",") 
                >> Seq.map (fun arr -> (int(arr[0]), int(arr[1]), int(arr[2]))))
    |> Seq.map Set.ofSeq

let rotations =
    [
        fun (x, y, z) -> (x, y, z);
        fun (x, y, z) -> (x, -z, y);
        fun (x, y, z) -> (x, -y, -z);
        fun (x, y, z) -> (x, z, -y);
        fun (x, y, z) -> (-x, z, y);
        fun (x, y, z) -> (-x, y, -z);
        fun (x, y, z) -> (-x, -z, -y);
        fun (x, y, z) -> (-x, -y, z);
        fun (x, y, z) -> (y, z, x);
        fun (x, y, z) -> (y, x, -z);
        fun (x, y, z) -> (y, -z, -x);
        fun (x, y, z) -> (y, -x, z);
        fun (x, y, z) -> (-y, x, z);
        fun (x, y, z) -> (-y, z, -x);
        fun (x, y, z) -> (-y, -x, -z);
        fun (x, y, z) -> (-y, -z, x);
        fun (x, y, z) -> (z, x, y);
        fun (x, y, z) -> (z, y, -x);
        fun (x, y, z) -> (z, -x, -y);
        fun (x, y, z) -> (z, -y, x);
        fun (x, y, z) -> (-z, y, x);
        fun (x, y, z) -> (-z, x, -y);
        fun (x, y, z) -> (-z, -y, -x);
        fun (x, y, z) -> (-z, -x, y);
    ]

let addCoords ((x1, y1, z1): Coord) ((x2, y2, z2): Coord): Coord =
    (x1+x2, y1+y2, z1+z2)

let addOffset (probe: Probe) (coord: Coord): Probe = 
    Set.map (addCoords coord) probe


let tryMatch ((known: Probe), (unknown: Probe)): Option<Probe * Coord> =
    let tryFindOffset (rotated: Probe): Option<Probe * Coord> =
        Seq.allPairs known rotated
        |> Seq.map (fun ((xl, yl, zl), (xr, yr, zr)) -> (xl - xr, yl - yr, zl - zr))
        |> Seq.map (fun offset -> (addOffset rotated offset), offset)
        |> Seq.tryFind (fun (offsetProbe, offset) -> 
            let count = Set.intersect offsetProbe known |> Set.count
            count >= 12)
    let rotated = rotations |> Seq.map (fun rotation -> Set.map rotation unknown)
    rotated
    |> Seq.map tryFindOffset
    |> Seq.tryFind Option.isSome
    |> Option.flatten

let solve (probes: Probe seq): (Probe * Coord) seq =
    let mutable solved = Seq.take 1 probes |> Seq.map (fun probe -> probe, (0, 0, 0)) |> List.ofSeq
    let mutable unsolved = Seq.tail probes |> Seq.toList
    while Seq.length unsolved <> 0 do
        printfn "unsolved length is %A" (Seq.length unsolved)
        printfn "solved length is %A" (Seq.length solved)
        let ((found, offset), orig) =
            Seq.allPairs (solved|> Seq.map (fun (l,_) -> l)) unsolved
            |> Seq.map (fun (l, r) -> tryMatch (l, r), r)
            |> Seq.find (fun (l, r) -> l.IsSome) |> (fun (l, r) -> Option.get l, r)
        unsolved <- unsolved |> List.filter ((<>) orig)
        solved <- (found, offset) :: solved
        printfn "Found %A %A" found offset
    solved

let solution = solve probes
let part1 = (Seq.fold (fun full (probe, offset) -> Set.union full probe) Set.empty solution)
printfn "%A" (Set.count part1)

let manhattan (((x1, y1, z1): Coord), ((x2, y2, z2): Coord)) = 
   abs(x1-x2) + abs(y1-y2) + abs(z1-z2)
let distances = solution |> Seq.map (fun (l, r) -> r)

let maxDistance =
    distances |> Seq.allPairs distances
    |> Seq.map manhattan
    |> Seq.max

printfn "%A" maxDistance