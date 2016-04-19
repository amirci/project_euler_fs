
let isMul n x = x % n = 0
let (<||>) f g x = f x || g x

seq { 1..999 }
|> Seq.filter (isMul 5 <||> isMul 3)
|> Seq.reduce (+)
|> printf "%d"
