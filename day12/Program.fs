open System.IO

type Path = string list
type CaveMap = Map<string, string list>

let lines = File.ReadLines("input")

let addConnection (left, right) (map: CaveMap) : CaveMap =
    map |> Map.change left (fun cur -> 
        match cur with 
        | Some(conns) -> Some(right :: conns)
        | None -> Some([right])
    )

let addConnections (map: CaveMap) (left, right): CaveMap =
    map
    |> addConnection (left, right)
    |> addConnection (right, left)

let caveMap =
    lines
    |> Seq.map (fun x -> 
        let line = x.Split("-")
        (line.[0], line.[1]))
    |> Seq.fold addConnections Map.empty

printfn "%A" caveMap

let isValid (path: Path) (candidate: string): bool =
    if List.contains candidate path then
        if candidate.ToLower() = candidate then false
        else true
    else true

let nextPaths (map: CaveMap) (pathend: string) (path: Path): Path list =
    let head = path.Head
    if head = pathend then [path]
    else 
        Map.find head map
        |> List.filter (isValid path)
        |> List.map (fun x -> x :: path)


let getPaths (caveMap: CaveMap) (pathstart: string) (pathend: string): Path list =
    let rec addPaths (paths: Path list) (caveMap: CaveMap): Path list =
        if List.forall (fun path -> List.head path = pathend) paths then paths
        else addPaths (List.collect (nextPaths caveMap pathend) paths) caveMap
    addPaths [[pathstart]] caveMap

let paths = getPaths caveMap "start" "end"

printfn "%A" paths.Length

let getPaths2 (caveMap: CaveMap) (pathstart: string) (pathend: string): Path list =
    let hasTwoSmallCaveVisits (path: Path): bool =
        path |> List.exists (fun cave -> ((List.filter ((=) cave) path |> List.length) > 1) && (cave.ToLower() = cave))

    let isValid (path: Path) (candidate: string): bool =
        if candidate = pathstart then false
        else if candidate.ToLower() = candidate then
            if (hasTwoSmallCaveVisits path) && ((List.filter ((=) candidate) path |> List.length) > 0) then false
            else true
        else true

    let nextPaths (map: CaveMap) (path: Path): Path list =
        let head = path.Head
        if head = pathend then [path]
        else 
            Map.find head map
            |> List.filter (isValid path)
            |> List.map (fun x -> x :: path)

    let rec addPaths (paths: Path list) (caveMap: CaveMap): Path list =
        if List.forall (fun path -> List.head path = pathend) paths then paths
        else addPaths (List.collect (nextPaths caveMap ) paths) caveMap
    
    addPaths [[pathstart]] caveMap

let paths2 = 
    getPaths2 caveMap "start" "end" 
printfn "%A" paths2.Length
