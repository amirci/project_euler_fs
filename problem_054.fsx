open System
open System.IO

type Player = | Player1 | Player2

let cards = ['0';'1';'2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A']

let cardOrder  c = cards |> List.findIndex ((=) c)
let highToLow' = cardOrder >> (-) 15
let highToLow (c, _) = c |> highToLow'

let freq = Array.groupBy fst >> Array.map (fun (n, v) -> (n, v.Length)) >> Array.sortBy (fun (c, l) -> 100 * l + highToLow' c)

let isInfix (hand: char array)  =
  let starts = hand.[0] |> cardOrder
  let ends   = hand.[4] |> cardOrder
  ends - starts = 4

let handValue str =
  let hand      = str |> Array.map (fun (s:string) -> (s.[0], s.[1]))
  let cardFreq  = freq hand
  let values    = hand |> Array.map fst |> Array.sortBy cardOrder
  let valuesH2L = values |> Array.rev |> String

  let isFlush = hand |> Array.map snd |> Array.distinct |> Array.length |> (=) 1 // same suit
  let isRoyal = values = [|'T'; 'J'; 'Q'; 'K'; 'A'|]
  let isStraight = values |> isInfix // consecutive

  match cardFreq with
  | h when isFlush && isRoyal     -> "900000"
  | h when isFlush && isStraight  -> "8" + valuesH2L
  | [|(c1, 4); (c2, 1)|]          -> "7000" + ([|c1; c2|] |> String)
  | [|(c1, 3); (c2, 2)|]          -> "6000" + ([|c1; c2|] |> String)
  | h when isFlush                -> "5" + valuesH2L
  | h when isStraight             -> "4" + valuesH2L
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

  let won = List.zip h1 h2 |> winner

  //printf "%s\n" (line |> String.concat " ")
  //printf "%O --- %O (%O)\n" h1 h2 won
  won

File.ReadAllLines(@"p054_poker.txt")
//|> Seq.take 5
|> Seq.map (fun s -> s.Split ' ' |> playHand)
|> Seq.filter ((=) Player1)
|> Seq.length
|> printf "Player 1 won %d"


