-- https://stackoverflow.com/questions/9754794/read-until-end-of-stream-in-haskell
import Control.Monad
import Data.List.Split
import System.Environment (getArgs)

import Text.Regex.TDFA

(~>) = flip (.)
interlayer :: Monad m => m a -> (a -> b) -> (b -> m c) -> m c
interlayer from main to = from >>= main ~> to

parseInt :: String -> Int
parseInt s = read s

type Match = (String, String, String, [String])

type Round = (Int, Int, Int)
maxRound (r1, g1, b1) (r2, g2, b2) = 
  (max r1 r2, max g1 g2, max b1 b2)

data Game = Game Int [Round] deriving (Show, Eq)
gId (Game i _) = i

sums :: [Game] -> Int
sums = sum . map gId

parse :: String -> Game 
parse s = let (_, _, _, [gId', whole]) = s =~ "Game ([0-9]+): (.*)" :: Match
              gId = parseInt gId'
              rounds = endBy ";" whole
              parseColors rnd = 
                      ( takeCol "red" rnd
                      , takeCol "green" rnd
                      , takeCol "blue" rnd )
                    where
                      takeCol :: String -> String -> Int
                      takeCol col rnd = 
                        let (_, _, _, parsed) = rnd =~ ("([0-9]+) " ++ col) :: Match 
                        in if length parsed == 0 then 0 else parseInt $ head parsed
            in
              Game gId $ map parseColors rounds


answer games = do
  Game gId rounds <- games
  return (gId, foldl1 maxRound rounds)


parts :: [() -> IO ()]
parts = [
  \() -> do input <- getContents
            let games = map parse $ lines input
            print $ sum $ do
              (gId, (red, green, blue)) <- answer games
              guard $ red <= 12 && green <= 13 && blue  <= 14
              return gId
        ,
  \() -> do input <- getContents
            let games = map parse $ lines input
            print $ sum $ do
              (gId, (red, green, blue)) <- answer games
              return $ red * green * blue
        ]

main = interlayer getArgs 
  ((!! 0) ~> parseInt ~> (flip (-) 1) ~> (parts !!)) 
  ($ ())
