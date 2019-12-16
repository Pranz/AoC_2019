
import qualified Data.Map.Strict as M
import           Control.Applicative
import           Data.List (break)

type Planet = String
type Orbit = (Planet, Planet)
type OrbitLookupMap = M.Map Planet Planet
type OrbitCountMap = M.Map Planet Int
type Maps = (OrbitLookupMap, OrbitCountMap)

parseOrbit :: String -> Orbit
parseOrbit str = 
    let (a, b) = break (== ')') str
    in (tail b,a)

indirectOrbitsForPlanet :: Planet -> Maps -> (Int, Maps)
indirectOrbitsForPlanet "COM" = (,) 0
indirectOrbitsForPlanet planet = do
    let (orbitsAround, orbitCount) = (fst, snd)
    cached <- M.lookup planet . orbitCount
    case cached of
        Just n -> (,) n
        Nothing -> do
            parent <- (M.! planet) . orbitsAround
            (parentOrbits, (orbitMap, previousOrbitCounts)) <- indirectOrbitsForPlanet parent
            let res = parentOrbits + 1
            let newOrbitCounts = M.insert planet res previousOrbitCounts
            return (res, (orbitMap, newOrbitCounts))

swap (a,b) = (b,a)
(.:) f g = \x y -> f (g x y)

part1 :: IO ()
part1 = do
    input <- lines <$> readFile "./aoc6_input"
    let orbits = M.fromList $ map parseOrbit input
    let (_, orbitCountMap) = 
            M.mapAccum (swap .: flip indirectOrbitsForPlanet) (orbits, M.empty) orbits
    print $ M.foldr (succ .: (+)) 0 orbitCountMap

type Distance = Int

orbitPath :: Planet -> Distance -> OrbitLookupMap -> M.Map Planet Distance
orbitPath "COM" distance = return $ M.fromList [("COM", distance)]
orbitPath planet distance = do
    parent <- (M.! planet)
    partPath <- orbitPath parent (distance + 1)
    return $ M.insert planet distance partPath

part2 :: IO ()
part2 = do
    input <- lines <$> readFile "./aoc6_input"
    let orbits = M.fromList $ map parseOrbit input
    let path1 = orbitPath "SAN" 0 orbits
    let path2 = orbitPath "YOU" 0 orbits
    let merged = M.intersectionWith ((pred . pred) .: (+)) path1 path2
    print $ foldr min 2000000 merged