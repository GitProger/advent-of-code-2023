import System.Environment (getArgs)

(~>) = flip (.)

parseInt :: String -> Int
parseInt s = read s

ints :: String -> [Int]
ints = map parseInt . words

diff :: [Int] -> [Int]
diff a = zipWith (-) (tail a) a

extraFwd l = if all (== 0) l then 0 else last l + (extraFwd $ diff l)
extraBck l = if all (== 0) l then 0 else head l - (extraBck $ diff l)

task ans = getContents >>= lines ~> map (ans . ints) ~> sum ~> print

parts :: [IO ()]
parts = [task extraFwd, task extraBck]
main = getArgs >>= (!! 0) ~> parseInt ~> (flip (-) 1) ~> (parts !!)
