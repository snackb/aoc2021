open System.IO

type PacketType<'a> = Literal of int | Operator of 'a seq
type Packet = { Version: int; Type: PacketType<Packet>}

let line = File.ReadLines("input") |> Seq.head

let hexToBinary (input: char) = 
    string input
    |> (fun x -> System.Convert.ToInt32(x, 16)

let parseList =
    List.toArray<char> >> System.String
    >> (fun x -> System.Convert.ToInt32(x, 2))

let recParsePacket (input: char list): Packet =
    let version = List.take 3 input |> parseList
    let type = List.skip 3 input |> List.take 3

