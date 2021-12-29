open System.IO
open System

type Coord = int * int
type Image = Set<Coord> * bool

let split (delim: String) (s: String) = s.Split(delim)

let txt = File.ReadAllText("input")

let splitted= split "\n\n" txt

let algo = Seq.toArray splitted[0] 
let image: Image =  
    splitted[1]
    |> split "\n"
    |> Seq.indexed
    |> Seq.map (fun (x, line) -> 
        Seq.indexed line
        |> Seq.map (fun (y, chr) -> 
            match chr with
            | '#' -> Some(x, y)
            | '.' -> None
            | _ -> raise (InvalidDataException())
        )
    )
    |> Seq.collect id |> Seq.filter Option.isSome 
    |> Seq.map Option.get
    |> Seq.fold (fun img coord -> Set.add coord img) Set.empty
    |> (fun x -> (x, false))

let addCoords ((x1, y1): Coord) ((x2, y2): Coord) =
    (x1+x2, y1+y2)

let toBinary: bool seq -> int = 
    Seq.map (function | true -> 1 | false -> 0)
    >> Seq.rev
    >> Seq.indexed
    >> Seq.fold (fun acc (i, digit) -> acc + (int((float(2)**i)) * digit)) 0

let getIndex inverted (image: Set<Coord>) (coord: Coord) =
    let adjCoords = 
        Seq.allPairs [-1; 0; 1] [-1; 0; 1]
        |> Seq.map (addCoords coord)
        |> Seq.map (fun coord -> Set.contains coord image)
        |> Seq.map (if inverted then not else id)
    toBinary adjCoords


let getAllCoords (image: Coord seq): Coord seq = 
    let maxx = Seq.map (fun (x, _) -> x) image |> Seq.max
    let minx = Seq.map (fun (x, _) -> x) image |> Seq.min
    let maxy = Seq.map (fun (_, y) -> y) image |> Seq.max
    let miny = Seq.map (fun (_, y) -> y) image |> Seq.min
    printfn "%A %A %A %A" maxx minx maxy miny
    seq {minx - 20 .. maxx + 20} |> Seq.allPairs (seq {miny - 20 .. maxy + 20})

let shouldOutput inverted index = 
    match Array.get algo index with
    | '#' -> if inverted then true else false
    | '.' -> if inverted then false else true
    | _ -> raise (InvalidDataException())


let getNextImage ((image, inverted): Image): Image =
    getAllCoords image
    |> Seq.filter (fun coord -> getIndex inverted image coord |> shouldOutput inverted)
    |> Seq.fold (fun img coord -> Set.add coord img) Set.empty
    |> (fun img -> (img, not(inverted)))

let (twotimes, inverted) = image |> getNextImage |> getNextImage

let rec repeatNTimes image n: Image = 
    match n with
    | 0 -> image
    | x -> repeatNTimes (getNextImage image) (n - 1)

printfn "%A %A" (Set.count twotimes) inverted

let (repeat50, _) = repeatNTimes image 50

printfn "%A" (Set.count repeat50)