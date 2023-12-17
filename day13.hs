import System.Environment (getArgs)
import Data.List.Split (endBy)

(~>) = flip (.)

parseInt :: String -> Int
parseInt s = read s

ints :: String -> [Int]
ints = map parseInt . words

transpose :: [[a]] -> [[a]]
transpose t = foldl1 (zipWith (++)) $ map (map (\i -> [i])) t

smap f = sum . map f

subList a b e = take (e - b) $ drop b a

isPalindrome :: [[Char]] -> Bool
isPalindrome sub = sub == reverse sub

isPalindrome' :: [[Char]] -> Bool
isPalindrome' sub = q == 2 -- counted twice
  where 
    q = sum $ zipWith diff sub $ reverse sub
    diff a b = foldl (\a c -> if c then a + 1 else a) 0 $ zipWith (/=) a b

horizontal :: ([[Char]] -> Bool) -> [[Char]] -> Int
horizontal test f = 
  let n = length f
      allSubsLeft  = [(0, i) | i <- [n, n - 1 .. 1]]
      allSubsRight = [(i, n) | i <- [n - 1, n - 2 .. 1]]

      left  = dropWhile (not . reflects) allSubsLeft
      right = dropWhile (not . reflects) allSubsRight

      mirrors = left ++ right
  in
    if mirrors == [] then 0 else mid $ head $ mirrors
    where
      mid (a, b) = (a + b) `div` 2
      reflects :: (Int, Int) -> Bool
      reflects (s, e) = 
        let sub = subList f s e 
        in (e - s) `mod` 2 == 0 && test sub

vertical t = (horizontal t) . transpose

solve :: ([[Char]] -> Bool) -> [ [[Char]] ] -> Int
solve t fields = 100 * (smap (horizontal t) fields) + (smap (vertical t) fields)
-- solve1 fields = map vertical fields

getFields :: IO [ [[Char]] ]
getFields = do
  fs <- getContents
  return $ map lines $ endBy "\n\n" fs

parts :: [IO ()]
parts = [do fs <- getFields
            print $ solve isPalindrome fs
        , 
         do fs <- getFields
            print $ solve isPalindrome' fs
        ]

main = getArgs >>= (!! 0) ~> parseInt ~> (flip (-) 1) ~> (parts !!)
