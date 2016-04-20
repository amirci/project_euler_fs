open System
open System.IO

type Player = | Player1 | Player2

let cards = ['0';'1';'2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A']
let cardOrder c = cards |> List.findIndex ((=) c)
let highToLow = cardOrder >> (-) 15

let isInfix (hand: char array) =
  let cardss = cards |> Array.ofList |> String

  hand
  |> String
  |> fun s -> cardss.Contains(s)
  |> (&&) (hand.Length = 5)

let handValue str =
  let freq      = Array.groupBy fst >> Array.map (fun (n, v) -> (n, v.Length)) >> Array.sortBy (fun (c, l) -> 100 * (5-l) + highToLow c)
  let hand      = str    |> Array.map (fun (s:string) -> (s.[0], s.[1]))
  let values    = hand   |> Array.map fst |> Array.sortBy cardOrder
  let valuesH2L = values |> Array.rev |> String
  let suits     = hand   |> Array.map snd

  let isFlush    = suits  |> Array.distinct |> Array.length |> (=) 1 // same suit
  let isStraight = values |> isInfix // consecutive
  let isRoyal    = values = [|'T'; 'J'; 'Q'; 'K'; 'A'|]

  match freq hand with
  | _ when isFlush && isRoyal     -> "900000"
  | _ when isFlush && isStraight  -> "8" + valuesH2L
  | [|(c1, 4); (c2, 1)|]          -> "7000" + ([|c1; c2|] |> String)
  | [|(c1, 3); (c2, 2)|]          -> "6000" + ([|c1; c2|] |> String)
  | _ when isFlush                -> "5" + valuesH2L
  | _ when isStraight             -> "4" + valuesH2L
  | [|(c1, 3); (c2, 1); (c3, 1)|] -> "300" + ([|c1; c2; c3|] |> String)
  | [|(c1, 2); (c2, 2); (c3, 1)|] -> "200" + ([|c1; c2; c3|] |> String)
  | [|(c1, 2); (c2, 1); (c3, 1); (c4, 1)|] -> "10" + String(c1, 1) + ([|c2; c3; c4|] |> String)
  | _ -> "0" + valuesH2L


let playHand line =
  let h1 = line |> Array.take 5 |> handValue |> List.ofSeq
  let h2 = line |> Array.skip 5 |> handValue |> List.ofSeq

  let rec winner = function
  | ((a, b)::xs) when cardOrder a > cardOrder b -> Player1
  | ((a, b)::xs) when cardOrder a = cardOrder b -> winner xs
  | _ -> Player2

  List.zip h1 h2 |> winner

File.ReadAllLines(@"p054_poker.txt")
|> Seq.map (fun s -> s.Split ' ' |> playHand)
|> Seq.filter ((=) Player1)
|> Seq.length
|> printf "Player 1 won %d"


