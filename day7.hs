import Control.Monad
import Data.List
import Data.Maybe
import Data.Function --
import System.Environment (getArgs)

type Match = (String, String, String, [String])

(~>) = flip (.)

parseInt :: String -> Int
parseInt s = read s

ints :: String -> [Int]
ints s = map parseInt $ words s

mostFrequent :: Ord a => [a] -> Maybe a
mostFrequent [] = Nothing
mostFrequent l = Just $ head $ last $ sortOn length $ group $ sort l

data CardType = None | High | OnePair | TwoPairs |Three | Full | Four | Five
  deriving (Show, Eq, Ord)

strength :: String -> CardType
strength hand = 
   let groups = sort $ map length $ group $ sort hand
   in case groups of 
      [5] -> Five 
      [1,4] -> Four
      [2,3] -> Full
      [1,1,3] -> Three
      [1,2,2] -> TwoPairs
      [1,1,1,2] -> OnePair
      [1,1,1,1,1] -> High
      otherwise -> None

parse :: String -> (String, Int)
parse line = let [a, b] = words line in (a, parseInt b)

points  card = fromJust $ elemIndex card $ ['2' .. '9'] ++ "TJQKA"
points' card = fromJust $ elemIndex card $ "J" ++ ['2' .. '9'] ++ "TQKA"

gains :: Ord a => (String -> a) -> [(String, Int)] ->  Int
gains order ps = 
  let sorted = sortOn (order . fst) ps 
  in sum $ zipWith (*) [1..] $ map snd sorted

enhance :: String -> String
enhance h = map (\c -> if c == 'J' then choose else c) h
  where choose = fromMaybe 'A' $ mostFrequent $ filter (/='J') h

parts :: [IO ()]
parts = [do input <- getContents
            let players = map parse $ lines input
            print $ gains (\x -> (strength x, map points x)) players
        ,
         do input <- getContents
            let players = map parse $ lines input
            print $ gains (\x -> (strength $ enhance x, map points' x)) players
        ]

main = getArgs >>= (!! 0) ~> parseInt ~> (flip (-) 1) ~> (parts !!)
