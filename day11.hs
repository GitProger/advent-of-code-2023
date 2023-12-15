import           System.Environment (getArgs)
import           Data.Function
import           Data.List          (sort, sortOn)  
import           Data.Tuple         (swap)
import qualified Data.Vector        as VC

(~>) = flip (.)

parseInt :: String -> Int
parseInt s = read s

ints :: String -> [Int]
ints = map parseInt . words

type Point = (Int, Int)

withInd :: [a] -> [(Int, a)]
withInd = zip [0..]

dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

parse :: String -> [Bool]
parse s = map (== '#') s 

transpose :: [[a]] -> [[a]]
transpose t = foldl1 (zipWith (++)) $ map (map (\i -> [i])) t

binarySearch :: Ord a => VC.Vector a -> a -> Int
binarySearch a x = bs (-1) $ length a
  where
    bs :: Int -> Int -> Int
    bs l r = 
      if l + 1 < r 
        then 
          let m = (l + r) `div` 2 in 
            if a VC.! m < x then bs m r else bs l m
        else r


galaxies :: [[Bool]] -> [Point]
galaxies field =
  concatMap 
    (\(i, row) -> 
      map (\(j, _) -> (i, j))
        $ filter snd $ withInd row)
    $ withInd field

rows f = map fst $ filter ((all not) . snd) $ withInd f
cols = rows . transpose

solve gap field =
  let g = galaxies field
      byRow = VC.fromList $ sort g
      byCol = VC.fromList $ sort $ map swap g
      orig  = sum [dist a b | a <- g, b <- g, a < b]
  in
    orig
     + (sum $ map (\rowI -> partition byRow rowI) $ rows field)
     + (sum $ map (\colI -> partition byCol colI) $ cols field)
  where
    -- `mid` row/col required empty
    partition a mid = 
      let b = binarySearch a (mid, 0)
      in b * ((length a) - b) * (gap - 1)


getField :: IO [[Bool]]
getField = getContents >>= return . map parse . lines

parts :: [IO ()]
parts = [do f <- getField
            print $ solve 2 f
        , 
         do f <- getField
            print $ solve 1000000 f
        ]
main = getArgs >>= (!! 0) ~> parseInt ~> (flip (-) 1) ~> (parts !!)
