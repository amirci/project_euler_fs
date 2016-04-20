open System
open System.IO

type Player = | Player1 | Player2

let cards = ['0';'1';'2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A']
let royal = cards |> List.skip 10 |> Array.ofList
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
  let isRoyal    = values = royal

  let (|FourOfAKind|_|)  = function | [|(_, 4); _|]            & v -> v |> Array.map fst |> String |> Some | _ -> None
  let (|ThreeOfAKind|_|) = function | [|(_, 3); (_, 1); _|]    & v -> v |> Array.map fst |> String |> Some | _ -> None
  let (|FullHouse|_|)    = function | [|(_, 3); (_, 2)|]       & v -> v |> Array.map fst |> String |> Some | _ -> None
  let (|TwoPairs|_|)     = function | [|(_, 2); (_, 2); _|]    & v -> v |> Array.map fst |> String |> Some | _ -> None 
  let (|OnePair|_|)      = function | [|(_, 2); (_, 1); _; _|] & v -> v |> Array.map fst |> String |> Some | _ -> None

  match freq hand with
  | _ when isFlush && isRoyal     -> "900000"
  | _ when isFlush && isStraight  -> "8" + valuesH2L
  | FourOfAKind s                 -> "7000" + s
  | FullHouse   s                 -> "6000" + s
  | _ when isFlush                -> "5" + valuesH2L
  | _ when isStraight             -> "4" + valuesH2L
  | ThreeOfAKind s                -> "300" + s
  | TwoPairs s                    -> "200" + s
  | OnePair s                     -> "10" + s
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


