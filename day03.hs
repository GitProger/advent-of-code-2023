import           Control.Monad
import           System.Environment (getArgs)
import           Data.Char
import           Data.List 
import qualified Data.Vector    as VC
import qualified Data.Map       as MP

(~>) = flip (.)
interlayer :: Monad m => m a -> (a -> b) -> (b -> m c) -> m c
interlayer from main to = from >>= main ~> to

parseInt :: String -> Int
parseInt s = read s

type Point = (Int, Int)
type Vector2D a = VC.Vector (VC.Vector a)

isSym :: Char -> Bool
isSym c = (c /= '.') && (not $ isDigit c)

ok :: Vector2D Char -> Point -> Bool
ok field (x, y) = 
  let (n, m) = (VC.length field, VC.length $ field ! 0) in
    x >= 0 && y >= 0 && x < n && y < m

neighbours :: Vector2D Char -> Point -> [Point]
neighbours field (x, y) = 
  let a = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], (dx /= 0 || dy /= 0)]
  in filter (ok field) a

(!) = (VC.!)

expand :: Vector2D Char -> Point -> (Point, Int)
expand field (x, y) =
    let l = expandSide (-1) y
        r = expandSide 1 y
    in
      ((x, l), parseInt $ map ((field ! x) !) [l..r])
        where
          expandSide :: Int -> Int -> Int
          expandSide side y = 
            if (not $ ok field (x, y)) || (not $ isDigit $ field ! x ! y) 
              then y - side
              else expandSide side $ y + side

symbolAdjacentInts :: Vector2D Char -> Point -> [(Point, Int)]
symbolAdjacentInts field (x, y) = 
  nub $ map (expand field) $ 
    filter (\(x, y) -> isDigit $ field ! x ! y) $ 
      neighbours field (x, y)

allSymbols :: Vector2D Char -> [Point]
allSymbols field = 
  let (n, m) = (VC.length field, VC.length $ field ! 0) in 
    [(x, y) | x <- [0..n - 1], y <- [0..m - 1], isSym $ field ! x ! y]

allNumbers :: Vector2D Char -> MP.Map Point Int
allNumbers field =
  let syms = allSymbols field in 
    foldl (\mp sym -> MP.union mp $ MP.fromList $ symbolAdjacentInts field sym) MP.empty syms
    -- MP.fromList $ foldl1 (++) (map (symbolAdjacentInts field) syms)

allNumbers' :: Vector2D Char -> [Int]
allNumbers' field =
  let syms = filter (\(x, y) -> (field ! x ! y) == '*') $ allSymbols field in 
    map (\sym ->
          let neighs = symbolAdjacentInts field sym in
            if length neighs == 2 
              then product (snd $ unzip neighs) else 0)
        syms


parts :: [() -> IO ()]
parts = [
  \() -> do input <- getContents
            let field = VC.fromList $ map VC.fromList $ lines input
            print $ sum $ MP.elems $ allNumbers field
        ,
  \() -> do input <- getContents
            let field = VC.fromList $ map VC.fromList $ lines input
            print $ sum $ allNumbers' field
        ]

main = interlayer getArgs
  ((!! 0) ~> parseInt ~> (flip (-) 1) ~> (parts !!)) 
  ($ ())
