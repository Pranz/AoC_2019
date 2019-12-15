import Prelude hiding (Left, Right)
import Control.Applicative
import qualified Data.Set as S
import qualified Data.Map.Strict as M


data Direction = Up | Down | Left | Right
    deriving (Show, Read, Eq, Ord)
type Path = [(Direction, Int)]

split :: Char -> String -> [String]
split _ "" = []
split sep (x:str) | sep == x = split sep str
split sep str = first : split sep rest
    where first = takeWhile (/= sep) str
          rest = dropWhile (/= sep) str

parseWire :: String -> Path
parseWire = map parsePath . split ','
    where parsePath (c:str) = (parseDir c, read str)
          parseDir c = case c of
            'U' -> Up
            'L' -> Left
            'D' -> Down
            'R' -> Right

getCoordsForPath :: Path -> [(Int, Int)]
getCoordsForPath = go (0,0) []
    where go pos acc [] = pos:acc
          go pos acc ((dir,0):rest) = go pos acc rest
          go pos@(x,y) acc ((dir,n):rest) = go newPos (pos:acc) ((dir,pred n):rest)
            where newPos = case dir of
                    Up -> (x, y-1)
                    Down -> (x, y+1)
                    Right -> (x + 1, y)
                    Left -> (x - 1, y)

veclen :: (Int, Int) -> Int
veclen (x,y) = abs x + abs y

main :: IO ()
main = part2
    where part1 = do
            (line1, line2) <- liftA2 (,) getLine getLine
            let [wire1, wire2] = map (S.fromList . getCoordsForPath . parseWire) [line1, line2]
            let intersections' = S.intersection wire1 wire2
            let intersections = S.delete (0,0) intersections'
            let distances = S.map veclen intersections
            print . S.lookupMin $ distances
          part2 = do
            (line1, line2) <- liftA2 (,) getLine getLine
            let [wire1, wire2] = map (M.fromList . flip zip [0..] . reverse . getCoordsForPath . parseWire) [line1, line2]
            let intersections' = M.intersectionWith (+) wire1 wire2
            let intersections = M.delete (0,0) intersections'
            print . foldr min 2000000 . M.elems $ intersections

