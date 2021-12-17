open System.IO
open System

type OperatorType = 
    | Sum
    | Mult
    | Min
    | Max
    | Gt
    | Lt
    | Eq

type PacketType = Literal of int64 | Operator of OperatorType * Packet seq 
    and Packet = { Version: int; Type: PacketType; Length: int}

let line = File.ReadLines("input") |> Seq.head

let hexToBinary (input: char) = 
    string input
    |> (fun x -> System.Convert.ToInt32(x, 16))
    |> (fun x -> System.Convert.ToString(x, 2).PadLeft(4, '0'))

let input = line |> Seq.collect hexToBinary |> Seq.toArray
printfn "%A" input

let parseInt: char[] -> int =
    System.String
    >> (fun x -> System.Convert.ToInt32(x, 2))

let parseInt64: char[] -> int64 =
    System.String
    >> (fun x -> System.Convert.ToInt64(x, 2))

let parseVariableInt (input: char array): int64 * int =
    let rec variableRec (input: char array): char array =
        match input[0] with
        | '0' -> input[1..4]
        | '1' -> Array.append input[1..4] (variableRec input[5..])
        | _ -> raise (InvalidDataException())
    let intChars = variableRec input
    let length = intChars.Length + (intChars.Length / 4)
    (intChars |> parseInt64, length)

let getVersion (input: char array) = input[0..2] |> parseInt

let rec recParsePacket (input: char array): Packet =
    let parseLiteral (input: char array): Packet =
        let version = getVersion input
        let inner, length = parseVariableInt input[6..]
        {Version = version; Type = Literal inner; Length = length + 6}

    let parseBitLengthOperator (op: OperatorType) (input: char array): Packet =
        let version = getVersion input
        let targetLength = input[7..21] |> parseInt
        let mutable index = 0
        let mutable packets = []
        while List.sumBy (fun x -> x.Length) packets < targetLength do
            packets <- (recParsePacket input[22 + index ..] :: packets)
            index <- index + packets.Head.Length
        { Version = version; Type = Operator (op, packets); Length = 22 + index}

    let parseCountOperator (op: OperatorType) (input: char array): Packet =
        let version = getVersion input
        let targetCount = input[7..17] |> parseInt
        let mutable index = 0
        let mutable packets = []
        while List.length packets < targetCount do
            packets <- (recParsePacket input[18 + index ..] :: packets)
            index <- index + packets.Head.Length
        { Version = version; Type = Operator (op, packets); Length = 18 + index}
    
    let parseOperator (op: OperatorType)(input: char array): Packet =
        let lengthType = input[6]
        match lengthType with 
        | '0' -> parseBitLengthOperator op input
        | '1' -> parseCountOperator op input
        | _ -> raise (InvalidDataException())

    let packetType = input[3..5] |> parseInt
    match packetType with 
    | 0 -> parseOperator Sum input
    | 1 -> parseOperator Mult input
    | 2 -> parseOperator Min input
    | 3 -> parseOperator Max input
    | 4 -> parseLiteral input
    | 5 -> parseOperator Gt input
    | 6 -> parseOperator Lt input
    | 7 -> parseOperator Eq input
    | _ -> raise (InvalidDataException())

let rec sumVersions (packet: Packet): int = 
    match packet.Type with
    | Literal _ -> packet.Version
    | Operator (_, xs) -> packet.Version + (Seq.sumBy sumVersions xs)

let packet = recParsePacket input
printfn "%A" (sumVersions packet)

let rec packetValue (packet: Packet) =
    let pairComparison (op: int64 -> int64 -> bool) (xs: Packet seq) =
        if op (packetValue (Seq.head xs)) (packetValue (Seq.last xs)) then 1 else 0
    match packet.Type with 
    | Literal lit -> lit
    | Operator (Sum, xs) -> Seq.sumBy packetValue xs
    | Operator (Mult, xs) -> Seq.fold (fun acc x -> acc * packetValue x) 1 xs
    | Operator (Min, xs) -> Seq.min (Seq.map packetValue xs)
    | Operator (Max, xs) -> Seq.max (Seq.map packetValue xs)
    | Operator (Gt, xs) -> pairComparison (<) xs // we get packets in reverse order
    | Operator (Lt, xs) -> pairComparison (>) xs
    | Operator (Eq, xs) -> pairComparison (=) xs

let finalValue = packetValue packet
printfn "%A" finalValue
