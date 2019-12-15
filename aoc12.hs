
import Control.Monad (forM_, mapM_)
import Control.Applicative (liftA2)

--- Types
type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)

type Body = (Position, Velocity)
type System = [Body] -> [Body]

--- Utility functions
zipVectors :: (a -> a -> r) -> (a, a, a) -> (a, a, a) -> (r, r, r)
zipVectors f (a, b, c) (a', b', c') = (f a a', f b b', f c c')

startingBodies :: [Body]
startingBodies =
    [ ((17, -9,   4),  (0,0,0))
    , (( 2,  2, -13),  (0,0,0))
    , ((-1,  5, - 1),  (0,0,0))
    , (( 4,  7,  -7),  (0,0,0))]

-- <x=17, y=-9, z=4>
-- <x=2, y=2, z=-13>
-- <x=-1, y=5, z=-1>
-- <x=4, y=7, z=-7>

alternateStartingBodies :: [Body]
alternateStartingBodies =
    [ ((-8, -10, 0), (0,0,0))
    , ((5, 5, 10), (0,0,0))
    , ((2, -7, 3), (0,0,0))
    , ((9, -8, -3), (0,0,0))]

ord2len :: Ordering -> Int
ord2len GT = -1
ord2len EQ =  0
ord2len LT =  1

--- Start main
main :: IO ()
main = part2

part1 :: IO ()
part1 = printStatus ((zip [0..1000] states) !! 1000)
    where printStatus (idx, state) = do
            putStrLn ("After " ++ show idx ++ " steps:")
            printBodies state
            putStrLn ("Total energy: " ++ show (totalEnergy state))
            putStrLn ""

part2 :: IO ()
part2 = do
    print . findCycles $ states

findCycles :: [[Body]] -> (Int, Int, Int)
findCycles bodies = head (map f states)
    where f (state:states) 
            = map g state
                where g body = (firstXCycle, firstYCycle, firstZCycle)
                                where ((x, y, z), (vx, vy, vz)) = body
                                      xCycles = hasCycle x vx (map getX bodies)
                                      yCycles = hasCycle y vy (map getY bodies)
                                      zCycles = hasCycle z vz (map getZ bodies)
                                      firstXCycle = fst . head . filter ((== True) . snd) $ (zip [0..] xCycles)
                                      firstYCycle = fst . head . filter ((== True) . snd) $ (zip [0..] yCycles)
                                      firstZCycle = fst . head . filter ((== True) . snd) $ (zip [0..] zCycles)
          getX ((x, _, _), (vx, _, _)) = (x, vx)
          getY ((_, y, _), (_, vy, _)) = (y, vy)
          getZ ((_, _, z), (_, _, vz)) = (z, vz)
          hasCycle u vu bodies = map (== (u, vu)) bodies
        

printBodies :: [Body] -> IO ()
printBodies = mapM_ printBody
    where printBody ((x,y,z), (vx, vy, vz)) = putStrLn . concat $
            [ "pos=<"
            , "x= ", show x, ", "
            , "y= ", show y, ", "
            , "z= ", show z, ">, "
            , "vel=<"
            , "x= ", show vx, ", "
            , "y= ", show vy, ", "
            , "z= ", show vz, ">" ]


states :: [[Body]]
states = iterate timeStep startingBodies

timeStep :: System
timeStep = applyVelocity . applyGravity

applyGravity :: System
applyGravity bodies = map applyGravity' bodies
    where applyGravity' body = foldr updateVelocity body diffs
            where updateVelocity (dx, dy, dz) (pos, (x, y, z)) = (pos, (x + dx, y + dy, z + dz))
                  diffs = let (pos, _) = body
                          in map (zipVectors (\p -> ord2len . compare p) pos) (map fst bodies)

applyVelocity :: System
applyVelocity = map applyVelocity'
    where applyVelocity' (pos, vel) = (zipVectors (+) pos vel, vel)

totalEnergy :: [Body] -> Int
totalEnergy = sum . map getEnergy
    where getEnergy = do
            k <- kineticEnergy
            p <- potentialEnergy
            return (p * k)
          kineticEnergy (_, vel) = absSum vel
          potentialEnergy (pos, _) = absSum pos
          absSum (x, y, z) = sum . map abs $ [x,y,z]
