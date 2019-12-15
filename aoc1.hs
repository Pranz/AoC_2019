
main :: IO ()
main = do
    part2_test
    part2

part1 :: IO ()
part1 = do
    input <- readFile "./aoc1_input"
    let masses = map read . lines $ input
    let fuel = map fuelRequired masses
    let totalFuel = sum fuel
    print totalFuel

part2 :: IO ()
part2 = do
    input <- readFile "./aoc1_input"
    let masses = map read . lines $ input
    let fuel = map fuelRequiredIterative masses
    let totalFuel = sum fuel
    print totalFuel

fuelRequired :: Int -> Int
fuelRequired = (max 0) . (subtract 2) . (`div` 3)

fuelRequiredIterative :: Int -> Int
fuelRequiredIterative = sum . tail . takeWhile (/= 0) . iterate fuelRequired