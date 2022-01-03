open System.IO
open System

type Direction = On | Off
type Range = int * int
type Voxel = Range * Range * Range
type Instruction = Direction * Voxel

let split (delim: String) (s: String) = s.Split(delim)

let lines = File.ReadAllLines("input")

let parseDirection: String -> Direction = function
    | "on" -> On
    | "off" -> Off

let parseRange (str: String): Range = 
    let eqarr = split "=" str
    let rangearr = split ".." eqarr[1]
    (int rangearr[0] , int rangearr[1])

let parse (line: String): Instruction = 
    let linearr = split " " line
    let direction = linearr[0] |> parseDirection
    let ranges = linearr[1] |> split "," |> Array.map parseRange
    (direction, (ranges[0], ranges[1], ranges[2]))

let instructions = Seq.map parse lines

let between min max x =
    x >= min && x <= max

let overlaps ((lmin, lmax): Range) ((rmin, rmax): Range): bool =
    between lmin lmax rmin
    || between lmin lmax rmax
    || between rmin rmax lmin
    || between rmin rmax lmax

let intersects (left: Voxel) (right: Voxel): bool =
    let (xl, yl, zl) = left
    let (xr, yr, zr) = right
    overlaps xl xr 
    && overlaps yl yr
    && overlaps zl zr

let subtractRange ((smin, smax): Range) ((tmin, tmax): Range): Range list = 
    if tmin < smin then [(tmin, smin - 1)] else []
    |> if smax < tmax then List.append [(smax + 1, tmax)] else id

let overlapBetween ((lmin, lmax): Range) ((rmin, rmax): Range): Range =
    (max lmin rmin , min lmax rmax)

/// Returns seq of voxels inside target after removing 
/// the volume of target which overlaps with the 
/// subtractor. 
let subtract (subtractor: Voxel) (target: Voxel): Voxel list =
    if not (intersects subtractor target) then List.singleton target
    else
    let (xs, ys, zs) = subtractor
    let (xt, yt, zt) = target

    let xsplits: Voxel list = 
        subtractRange xs xt 
        |> List.map (fun xrange -> (xrange, yt, zt))

    let xoverlap = overlapBetween xs xt

    let ysplits: Voxel list = 
        subtractRange ys yt 
        |> List.map (fun yrange -> (xoverlap, yrange, zt))
    
    let yoverlap = overlapBetween ys yt

    let zsplits: Voxel list =
        subtractRange zs zt 
        |> List.map (fun zrange -> (xoverlap, yoverlap, zrange))

    xsplits |> List.append ysplits |> List.append zsplits


let apply (voxels: Voxel list) ((direction, voxel): Instruction): Voxel list = 
    voxels
    |> List.collect (subtract voxel)
    |> match direction with 
        | On -> List.append (List.singleton voxel)
        | Off -> id

let p1 =
    instructions
    |> Seq.filter (fun (inst, ((x, _), _, _)) -> x <= 50 && x >= -50)
    |> Seq.fold apply List.empty

let getNumberOn (((xmin, xmax), (ymin, ymax), (zmin, zmax)): Voxel): int64 = 
    int64 (xmax + 1 - xmin) *
    int64 (ymax + 1 - ymin) * 
    int64 (zmax + 1 - zmin)

printfn "%A" (Seq.sumBy getNumberOn p1)

let p2 =
    instructions
    |> Seq.fold apply List.empty

printfn "%A" (Seq.sumBy getNumberOn p2)