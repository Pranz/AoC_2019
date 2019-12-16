
import Control.Monad (mapM_)

width  = 25 :: Int
height = 6  :: Int
area = width * height

groupByLength :: Int -> [a] -> [[a]]
groupByLength n [] = []
groupByLength n ls = take n ls : groupByLength n (drop n ls)

mergePixel :: Char -> Char -> Char
mergePixel '0' _ = '0'
mergePixel '1' _ = '1'
mergePixel '2' p = p

mergeLayers :: [String] -> String
mergeLayers layers = map (foldr mergePixel '2') (image layers)
    where image xs = map head xs : image (map tail xs)

decodeImage :: String -> String
decodeImage str = mergeLayers $ groupByLength area str

part1 :: IO ()
part1 = do
    input <- readFile "./aoc8_input"
    let layers = groupByLength area input
    let layersWithoutZeroes = map (filter (/= '0')) layers
    let layerWithFewestZeroes = foldr (\l1 l2 -> 
                                    case length l1 > length l2 of
                                        True  -> l1
                                        False -> l2
                                    ) "" layersWithoutZeroes
    let num1digits = length $ filter (== '1') layerWithFewestZeroes
    let num2digits = length $ filter (== '2') layerWithFewestZeroes
    print $ num1digits * num2digits

part2 :: IO ()
part2 = do
    input <- readFile "./aoc8_input"
    let img = decodeImage input
    let f x = case x of
                '1' -> '#'
                _ -> ' '
    let rows = take height . groupByLength width . map f $ img
    mapM_ putStrLn rows
