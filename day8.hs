import Control.Monad
import Data.List.Split
import System.Environment (getArgs)
import Text.Regex.TDFA
import qualified Data.Map as MP

type Match = (String, String, String, [String])

(~>) = flip (.)

parseInt :: String -> Int
parseInt s = read s

ints :: String -> [Int]
ints s = map parseInt $ words s

parse :: String -> (String, (String, String))
parse s = let (_, _, _, [f, l, r]) = s =~ "([A-Z]+) = \\(([A-Z]+), ([A-Z]+)\\)" :: Match
  in (f, (l, r))


dfs :: MP.Map String (String, String) -> String -> String -> (String -> Bool) -> Int -> Int
dfs graph c cur isEnd depth =
  if isEnd cur
    then depth
    else dfs graph (tail c) 
             ((if head c == 'L' then fst else snd) $ graph MP.! cur)
             isEnd $ depth + 1

getInputs = do
  instr <- getLine
  _ <- getLine
  input <- getContents
  let graph = MP.fromList $ map parse $ lines input
  return (instr, graph)

parts :: [IO ()]
parts = [do (instr, graph) <- getInputs
            print $ dfs graph (cycle instr) "AAA" (== "ZZZ") 0
        ,
         do (instr, graph) <- getInputs
            let starts = filter ((== 'A').last) $ MP.keys graph
            print $ foldl1 lcm 
                  $ map (\st -> dfs graph (cycle instr) st ((== 'Z').last) 0) 
                  starts
        ]

main = getArgs >>= (!! 0) ~> parseInt ~> (flip (-) 1) ~> (parts !!)
