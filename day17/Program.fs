open System.IO
open System

// let line = File.ReadLines("input") |> Seq.head

type Coord = int * int

let trajectory x y: Coord seq = 
    let mutable (xc, yc) = (0, 0)
    let mutable xvel = x
    let mutable yvel = y

    seq {
        while true do
            xc <- xc + xvel
            yc <- yc + yvel
            xvel <- xvel - (if xvel = 0 then 0 else xvel / abs(xvel))
            yvel <- yvel - 1
            yield (xc, yc) 
    }


let between a b i = i <= (max a b) && i >= (min a b)

let maxy minxc maxxc minyc maxyc =
    let trajectories = 
        seq {1..2000}
        |> Seq.allPairs (seq { 1..maxxc} )
        |> Seq.map (fun (x, y) -> trajectory x y )
        |> Seq.map (fun traj -> Seq.takeWhile (fun (x, y) -> x < maxxc + 1 && y > (min maxyc minyc) - 1) traj |> Seq.toList) |> Seq.toList
    let filteredTrajectories = 
        trajectories
        |> Seq.filter (fun traj -> Seq.exists (fun (x, y) -> (between minxc maxxc x) && (between maxyc minyc y)) traj) |> Seq.toList
    printfn "%A" (filteredTrajectories |> Seq.filter (Seq.isEmpty >> not))
    filteredTrajectories
    |> Seq.map (fun traj -> Seq.maxBy (fun (_, y) ->  y) traj)
    |> Seq.map (fun (x, y) -> y)
    |> Seq.max

//printfn "%A" (maxy 195 238 -93 -67)

let numVels minxc maxxc minyc maxyc =
    let trajectories = 
        seq {-100..100}
        |> Seq.allPairs (seq { 1..maxxc} )
        |> Seq.map (fun (x, y) -> trajectory x y )
        |> Seq.map (fun traj -> Seq.takeWhile (fun (x, y) -> x < maxxc + 1 && y > (min maxyc minyc) - 1) traj |> Seq.toList) |> Seq.toList
    let filteredTrajectories = 
        trajectories
        |> Seq.filter (fun traj -> Seq.exists (fun (x, y) -> (between minxc maxxc x) && (between maxyc minyc y)) traj) |> Seq.toList
    printfn "%A" (filteredTrajectories |> Seq.filter (Seq.isEmpty >> not))
    filteredTrajectories |> Seq.length

printfn "%A" (numVels 195 238 -93 -67)
    