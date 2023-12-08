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

ints :: String -> [Int]
ints s = map parseInt $ words s


solve :: Double -> Double -> [Double]
solve t record = -- x * (t - x) > record
  map (\s -> (t + s * sqrt(t ^ 2 - 4 * record)) / 2) [-1, 1]

count :: Int -> Int -> Int
count tm record = 
  let [l, r] = solve (fromIntegral tm) (fromIntegral record)
  in if isNaN l || isNaN r then 0 else floor r - ceiling l + 1

merge :: [Int] -> Int
merge x = foldr1 m1 x 
  where m1 a b = a * (10 ^ (ceiling (log (fromIntegral b) / log 10))) + b
  -- weak way: read $ concatMap show x

parts :: [IO ()]
parts = [do input <- getContents
            let [times, dists] = map ((map parseInt) . tail . words) $ lines input
            print $ product $ zipWith count times dists
        ,
         do input <- getContents
            let [times, dists] = map ((map parseInt) . tail . words) $ lines input
            print $ count (merge times) (merge dists)
        ]

main = getArgs >>= (!! 0) ~> parseInt ~> (flip (-) 1) ~> (parts !!)
