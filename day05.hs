import Control.Monad
import Data.List.Split
import System.Environment (getArgs)
import System.Exit
import Text.Regex.TDFA
import Data.Maybe
import qualified Data.Map as MP

type Match = (String, String, String, [String])

(~>) = flip (.)

parseInt :: String -> Int
parseInt s = read s

ints :: String -> [Int]
ints s = map parseInt $ words s

type Range = (Int, Int)

inRange x (b, e) = b <= x && x < e
subset (a, b) (a', b') = a' <= a && b <= b'
cutting (a, b) (c, _) = (a < c) && (c < b)

type Mapping = ((String, String), MP.Map Range Range)


search :: [Mapping] -> String -> (MP.Map Range Range, String)
search (first:rest) key = 
  let ((f, t), m) = first
  in if f == key then (m, t) else search rest key

mapped :: Int -> MP.Map Range Range -> Int
mapped id' map' = 
  let ans = dropWhile (not.(inRange id')) $ MP.keys map'
  in if ans == [] then id' else fst (map' MP.! (head ans)) + id' - fst (head ans)

solve1 :: [Mapping] -> String -> String -> Int -> Int
solve1 mappings keyStart keyEnd s = do
  if keyStart == keyEnd then s else
    let (map', nextKey) = search mappings keyStart 
    in solve1 mappings nextKey keyEnd $ mapped s map'

--------------------------------------------------------------------
--                      from  MP.elems
mappedRange :: Range -> (Range, Range) -> Range
mappedRange (a, b) (from@(x, _), to@(x', _))  -- (a, b) \subset from
  | (a, b) `subset` from = (a + x' - x, b + x' - x)
  | otherwise            = error "not a subset"

mappedRange' :: MP.Map Range Range -> Range -> Range -> Range
mappedRange' mp (a, b) from =
  let to = mp MP.!? from 
  in if to == Nothing then (a, b) else mappedRange (a, b) (from, fromJust to)

mappedTable :: MP.Map Range Range -> Range -> [Range]
mappedTable tab r
  | inside /= [] = map mapsTo inside -- == mapsTo $ head inside, as len(inside) obviously = 1  
  | left   /= [] = mappedTable tab left1 ++ mappedTable tab right1
  | right  /= [] = mappedTable tab left2 ++ mappedTable tab right2
  | otherwise = [r]
  where keys = MP.keys tab
        mapsTo = mappedRange' tab r
        inside = filter (r `subset`) $ keys
        left   = filter (`cutting` r) keys
        right  = filter (r `cutting`) keys

        (a, b) = r

        left1  = (a, cut1)
        right1 = (cut1, b)
        cut1   = snd $ head $ left

        left2  = (a, cut2)
        right2 = (cut2, b)
        cut2   = fst $ head $ right

solve2 :: [Mapping] -> String -> String -> [Range] -> Int
solve2 mappings keyStart keyEnd seeds =
  if keyStart == keyEnd 
    then minimum $ map fst seeds
    else
      let (map', nextKey) = search mappings keyStart 
      in solve2 mappings nextKey keyEnd $ concat $ map (mappedTable map') seeds


parseMapping :: String -> Mapping
parseMapping s =
  let (name:dat) = lines s 
      (_, _, _, [f, t]) = name =~ "(.*)-to-(.*) map:" :: Match
      mapping = MP.fromList $ map 
        (\[dst, src, ln] -> ((src, src + ln), (dst, dst + ln)))
        $ map ints dat
  in ((f, t), mapping)

getInputs :: IO ([Int], [Mapping])
getInputs = do
  inp <- getLine
  _ <- getLine
  let seeds = map parseInt $ tail $ words inp
  rest <- getContents
  let mappings = map parseMapping $ endBy "\n\n" rest
  return (seeds, mappings)



parts :: [IO ()]
parts = [do (seeds, mappings) <- getInputs
            print $ minimum $ map (solve1 mappings "seed" "location") seeds
        ,
         do (seeds, mappings) <- getInputs
            let seeds' = map (\[a, b] -> (a, a + b)) $ chunksOf 2 seeds
            print $ solve2 mappings "seed" "location" seeds'
        ]

main = getArgs >>= (!! 0) ~> parseInt ~> (flip (-) 1) ~> (parts !!)
