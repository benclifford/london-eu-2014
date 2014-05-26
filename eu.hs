import Data.Function
import Data.List
import Data.Traversable

data Parties = Labour | Conservative | UKIP | Green | LibDem
  deriving (Enum, Show, Eq)

results = [ (Labour, 806959),
            (Conservative, 495639),
            (UKIP, 371133),
            (Green, 196419),
            (LibDem, 148013)
          ]

seats = 8

calcMore = 4

seatRange = [1..(seats + calcMore)]

main = do
  putStrLn "D'Hondt calculations, London 2014 European Election"

  let r = [ (party, count `div` seat) | (party, count) <- results, seat <- seatRange ]

  let sr = sortBy ((flip compare) `on` snd) r

  let nsr = sr `zip` [1..]

  putStrLn "Elected: "
  printRes $ take seats sr

  putStrLn "Next up: "
  printRes $ take calcMore $ drop seats sr

  putStrLn "Table: "

  putStrLn "<table border=1>"
  putStrLn "<tr><th>Party</th><th>Count</th>"
  for seatRange $ \seat -> putStr $ "<th>/"++(show seat)++"</th>"
  putStrLn "</tr>"

  for results $ \(party, count) -> do
    putStrLn "<tr>"
    putStr $ "<td>" ++(show party) ++ "</td>"
    putStr $ "<td>" ++(show count) ++ "</td>"
    for seatRange $ \seat -> do
      let vps = count `div` seat
      let pos = lookup (party, vps) nsr
      let style =
           case pos of
             Just n | n <= seats -> "bgcolor=\"#FFFF00\""
             _ -> ""
      let aug = 
           case pos of 
             Just n | n <= (seats + calcMore) -> " (" ++ show n ++ ")"
             _ -> ""
      putStr $ "<td " ++ style ++ ">" ++ (show vps) ++ aug ++ "</td>"
   
    putStrLn "</tr>"

  putStrLn "</table>"

printRes rs = do
  for rs $ \r -> print r

