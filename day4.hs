import Control.Monad
import Data.List.Split
import System.Environment (getArgs)
import Text.Regex.TDFA

type Match = (String, String, String, [String])

(~>) = flip (.)
interlayer :: Monad m => m a -> (a -> b) -> (b -> m c) -> m c
interlayer from main to = from >>= main ~> to

parseInt :: String -> Int
parseInt s = read s

data Card = Card Int [Int] [Int] deriving Show

parse :: String -> Card
parse s = 
    let (_, _, _, [cId', l', r']) = s =~ "Card +([0-9]+): ([^|]*) \\| (.*)" :: Match
        cId = parseInt cId'
        ints line = map parseInt $ words line
        (win, my) = (ints l', ints r')
    in Card cId win my

matches :: Card -> Int
matches (Card _ win my) = length (filter (\x -> elem x win) my)

points :: Card -> Int
points c = let cnt = matches c in 
  if cnt == 0 then 0 else 2 ^ (cnt - 1)

copyCards :: [Card] -> [Int] -- i_th card beats (ans_i)-1 cards and itself
copyCards [] = []
copyCards (h:t) = 1 + sum (take (matches h) ans) : ans
  where ans = copyCards t

parts :: [() -> IO ()]
parts = [
  \() -> do input <- getContents
            let cards = map parse $ lines input
            print $ sum $ map points cards
        ,
  \() -> do input <- getContents
            let cards = map parse $ lines input
            print $ sum $ copyCards cards
        ]

main = interlayer getArgs 
  ((!! 0) ~> parseInt ~> (flip (-) 1) ~> (parts !!)) 
  ($ ())
