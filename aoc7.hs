
import IntCode
import Data.List (permutations)
import qualified Data.Map.Strict

test1 :: [Int]
test1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
test1phase :: [Int]
test1phase = [4,3,2,1,0]

test2 :: [Int]
test2 = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
test2phase :: [Int]
test2phase = [0,1,2,3,4]

mainProg :: [Int]
mainProg = [3,8,1001,8,10,8,105,1,0,0,21,30,47,60,81,102,183,264,345,426,99999,3,9,1002,9,5,9,4,9,99,3,9,1002,9,5,9,1001,9,4,9,1002,9,4,9,4,9,99,3,9,101,2,9,9,1002,9,4,9,4,9,99,3,9,1001,9,3,9,1002,9,2,9,101,5,9,9,1002,9,2,9,4,9,99,3,9,102,4,9,9,101,4,9,9,1002,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,99]

runWithInput = flip runMachine'
runWithInputFull = flip runMachineFullOutput

prog = mainProg

-- Part 1

part1 :: IO ()
part1 = do
    print $ foldr max (-99999999) tryAllPhases

tryAllPhases = do
    phase <- permutations [0,1,2,3,4]
    return $ runWithPhases phase

runWithPhases :: [Int] -> Int
runWithPhases phases = do
    let sequence = do
        out1 <- runWithInput [phases !! 0, 0]
        out2 <- runWithInput [phases !! 1, out1]
        out3 <- runWithInput [phases !! 2, out2]
        out4 <- runWithInput [phases !! 3, out3]
        runWithInput [phases !! 4, out4]
    sequence prog

-- Part 2

test3 :: [Int]
test3 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

runFeedback :: [Int] -> Int
runFeedback phases = last out5
    where in1 = (phases !! 0) : 0 : out5
          in2 = (phases !! 1) : out1
          in3 = (phases !! 2) : out2
          in4 = (phases !! 3) : out3
          in5 = (phases !! 4) : out4
          out1 = reverse $ runWithInputFull in1 prog
          out2 = reverse $ runWithInputFull in2 prog
          out3 = reverse $ runWithInputFull in3 prog
          out4 = reverse $ runWithInputFull in4 prog
          out5 = reverse $ runWithInputFull in5 prog

tryAllPhases' = do
    phase <- permutations [5, 6, 7, 8, 9]
    return $ runFeedback phase

part2 :: IO ()
part2 = do
    print $ foldr max (-99999999) tryAllPhases'