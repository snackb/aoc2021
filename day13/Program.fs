open System.IO

type Coord = int*int

let lines = File.ReadLines("input")

let dotsList, foldsList =
    lines
    |> Seq.filter ((<>) "")
    |> Seq.toList
    |> List.partition (fun line -> System.Char.IsNumber(line.[0]))

let dots =
    dotsList
    |> List.map (fun dot -> 
        let split = dot.Split(",")
        (int(split.[0]), int(split.[1])))
    |> List.fold (fun dotsmap coord -> Set.add coord  dotsmap) Set.empty

let folds =
    foldsList
    |> List.map (fun x -> x.Substring(11).Split("="))
    |> List.map (fun arr -> (arr.[0], int(arr.[1])))

printfn "%A" folds

let applyFold (axis: string) (distance: int) (dots: Set<Coord>) =
    let processDot (dots: Set<Coord>) ((dotx, doty): Coord) : Set<Coord> =
        match axis with
        | "x"-> 
            if dotx > distance then
                dots |> Set.remove (dotx, doty) |> Set.add ((2*distance) - dotx, doty)
            else
                dots
        | "y" -> 
            if doty > distance then
                dots |> Set.remove (dotx, doty) |> Set.add (dotx, (2*distance) - doty)
            else
                dots

    dots 
    |> Set.fold processDot dots

let axis, distance = folds.Head
let afterFold = applyFold axis distance dots

printfn "%A" afterFold.Count

let fullFolds =
    folds
    |> List.fold (fun dots (axis, distance) -> applyFold axis distance dots) dots

let maxx = Set.fold (fun acc (x, _) -> if x > acc then x else acc ) 0 fullFolds
let maxy = Set.fold (fun acc (_, y) -> if y > acc then y else acc ) 0 fullFolds

printfn "%A" (maxx, maxy)

let arr = 
    Array2D.create maxy maxx " "
    |> Array2D.mapi (fun y x _ -> if Set.contains (x, y) fullFolds then "*" else " ")


for i in seq {0..maxy} do
    for j in seq {0..maxx} do
        if Set.contains (j, i) fullFolds then printf "*" 
        else printf " " 
    printfn ""
 
