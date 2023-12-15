import System.Environment (getArgs)

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

galaxies :: [[Bool]] -> [Point]
galaxies field =
  concatMap 
    (\(i, row) -> 
      map (\(j, _) -> (i, j))
        $ filter snd $ withInd row)
    $ withInd field

rows f = map fst $ filter ((all not) . snd) $ withInd f
cols = rows . transpose

expansion field = 
  transpose $ expand (cols field) 
    $ transpose $ expand (rows field)
      field
  where
    expand :: [Int] -> [[Bool]] -> [[Bool]]
    expand empty f = 
      concatMap (\(i, row) -> if elem i empty then [row, row] else [row]) $ withInd f

solve field = sum [dist a b | a <- g, b <- g, a < b]
  where g = galaxies $ expansion field


getField :: IO [[Bool]]
getField = getContents >>= return . map parse . lines

parts :: [IO ()]
parts = [do f <- getField
            print $ solve f
        , 
         do print "hi 2"
        ]
main = getArgs >>= (!! 0) ~> parseInt ~> (flip (-) 1) ~> (parts !!)
