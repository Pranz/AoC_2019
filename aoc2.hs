import IntCode
import Control.Monad.State.Lazy
import Data.Array

mainProgram :: [Int]
mainProgram = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,13,19,1,9,19,23,1,6,23,27,2,27,9,31,2,6,31,35,1,5,35,39,1,10,39,43,1,43,13,47,1,47,9,51,1,51,9,55,1,55,9,59,2,9,59,63,2,9,63,67,1,5,67,71,2,13,71,75,1,6,75,79,1,10,79,83,2,6,83,87,1,87,5,91,1,91,9,95,1,95,10,99,2,9,99,103,1,5,103,107,1,5,107,111,2,111,10,115,1,6,115,119,2,10,119,123,1,6,123,127,1,127,5,131,2,9,131,135,1,5,135,139,1,139,10,143,1,143,2,147,1,147,5,0,99,2,0,14,0]

targetOutput :: Int
targetOutput = 19690720

correctProgram :: Int -> Int ->IntCode ()
correctProgram n k = do
    modify (// [(1, n), (2, k)])
    return ()

findRightCorrection :: [Int] -> (Int, Int, Int)
findRightCorrection ls = (rightNoun, targetOutput - programOutput, realProgramOutput)
    where startingMemory = listArray (0, subtract 1 . length $ ls) ls
          execProgram noun =
                            let output = evalState (correctProgram noun 0 >> runUntilHalt 0) startingMemory
                            in (noun, output)
          increasingNouns = fmap execProgram [1..]
          (rightNoun, (programOutput:_)) = last . takeWhile (\(noun, (output:restMemory)) -> output <= targetOutput) $ increasingNouns
          realProgramOutput = head $ evalState (correctProgram rightNoun (targetOutput - programOutput) >> runUntilHalt 0) startingMemory

main :: IO ()
main = do
    let ls = mainProgram
    let res = evalState (correctProgram 1 2 >> runUntilHalt 0) (listArray (0, subtract 1 . length $ ls) ls)
    print res