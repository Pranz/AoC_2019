
import Data.List (sortBy, groupBy)
import Data.Ratio
import Control.Monad (forM_, mapM_)

(.:) f g = \x y -> f (g x y)

data OverflowingRatio a = NegInf | Some (Bool, Ratio a) | PosInf
    deriving (Eq, Show)

getRatio :: (Int, Int) -> OverflowingRatio Int
getRatio (x,y) = case x of
    0 -> if y >= 0 then PosInf
                   else NegInf
    x' -> Some (x' < 0, (y % x'))

flipOrder EQ = EQ
flipOrder LT = GT
flipOrder GT = LT

compareRatio :: (Ord a, Integral a) => OverflowingRatio a -> OverflowingRatio a -> Ordering
compareRatio x y | x == y = EQ
compareRatio NegInf _ = LT
compareRatio PosInf _ = GT
compareRatio (Some (p, a)) (Some (p',  b)) = case p `compare` p' of
    EQ -> (a `compare` b)
    ordering -> ordering
compareRatio x y = flipOrder (compareRatio y x)

concatWithCoords :: [(Int, [a])] -> [(Int, Int, a)]
concatWithCoords = concat . map (do
    y <- fst
    row <- zip [(0 :: Int)..] . snd
    return $ map (\(x, c) -> (x, y, c)) row)

isAsteroid :: (Int, Int, Char) -> Bool
isAsteroid (_,_,'#') = True
isAsteroid _ = False


visibleAsteroidsGroup :: [(Int, Int, Char)] -> (Int ,Int, Char) -> [[(Int, Int, OverflowingRatio Int)]]
visibleAsteroidsGroup asteroids (x, y, _) = groupsOfEqualDirs
    where filtered = filter (/= (x,y,'#')) asteroids
          relativeCoords = map (\(x', y', _) -> (x' - x, y' - y)) filtered
          dirs = map (\(x, y) -> (x, y, getRatio (x,y))) relativeCoords
          sortedDirs = sortBy (withThird compareRatio) dirs
          groupsOfEqualDirs = groupBy (withThird (==)) sortedDirs

visibleAsteroids :: [(Int, Int, Char)] -> (Int, Int, Char) -> (Int, Int, Int)
visibleAsteroids asteroids (x, y, _) = (x, y, length groupsOfEqualDirs)
    where filtered = filter (/= (x,y,'#')) asteroids
          relativeCoords = map (\(x', y', _) -> (x' - x, y' - y)) filtered
          dirs = map getRatio relativeCoords
          sortedDirs = sortBy compareRatio dirs
          groupsOfEqualDirs = groupBy (==) sortedDirs

maxTriple t1@(_,_,x) t2@(_,_,y) = if x >= y then t1
                                            else t2

withThird f (_,_,x) (_,_,y) = f x y

part1 :: IO ()
part1 = do
    input <- readFile "./aoc10_input"
    let grid = concatWithCoords . zip [0..] . lines $ input
    let asteroids = filter isAsteroid grid
    print . foldr maxTriple (0,0,0) . map (visibleAsteroids asteroids) $ asteroids
    print . map (map (\(x,y,_) -> (x,y))) . filter ((> 1) . length) . visibleAsteroidsGroup asteroids $ (1,2,'#')


monitoringStation :: (Int, Int)
monitoringStation = (25, 31)



part2 :: IO ()
part2 = do
    input <- readFile "./aoc10_input"
    let grid = concatWithCoords . zip [0..] . lines $ input
    let asteroids = filter isAsteroid grid
    let (relX, relY) = monitoringStation
    -- let asteroidsInOrder = map (sortBy (withThird compare)) . map (map (\(x,y,_) -> (x, y, (x*x) + (y*y)))) . visibleAsteroidsGroup asteroids $ (relX,relY,'#')
    let asteroidsInOrder = map (sortBy (withThird compare)) . map (map (\(x,y,_) -> (relX + x, relY + y, (x*x) + (y*y)))) . visibleAsteroidsGroup asteroids $ (relX,relY,'#')
    let asteroidAngles = map (map (\(x, y, _) -> atan2 (fromIntegral x)  (fromIntegral y))) asteroidsInOrder :: [[Double]]
    forM_ asteroidAngles print
    vaporizeTurn asteroidsInOrder 1

vaporizeTurn :: [[(Int, Int, Int)]] -> Int -> IO ()
vaporizeTurn [] _ = return ()
vaporizeTurn ls counter = do
    printTurn turn
    vaporizeTurn next (counter + length turn)
        where printTurn :: [(Int, (Int, Int, Int))] -> IO ()
              printTurn = mapM_ printVap
              printVap (idx, (x,y,_)) = putStrLn ("Vaporized n: " ++ show idx ++ " " ++ show (x,y))
              turn = zip [counter..] . map head $ ls
              next = filter (/= []) . map tail $ ls

main :: IO ()
main = part2