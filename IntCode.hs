module IntCode where

import Data.Array
import Control.Monad.State.Lazy
import Control.Monad.Identity
import Control.Applicative
import Control.Monad (mapM)
import Control.Monad.Fail

type IntCode a = State (Array Int Int) a

data OpCode
    = Add
    | Mul
    | Read
    | Write
    | OpJumpTrue
    | OpJumpFalse
    | OpLT
    | OpEQ
    | H

data Mode = Position | Immediate

data StatusCode = Ok | Stop | Output Int | DidRead | Branch Int
    deriving (Show, Eq)

parseCodeAndMode :: Int -> ([Mode], OpCode)
parseCodeAndMode x = ( [ parseMode ((x `div` 100  ) `mod` 10)
                       , parseMode ((x `div` 1000 ) `mod` 10)
                       , parseMode ((x `div` 10000) `mod` 10)]
                     , parseOpCode . (`mod` 100) $ x)

parseMode :: Int -> Mode
parseMode 0 = Position
parseMode 1 = Immediate

parseOpCode :: Int -> OpCode
parseOpCode 1 = Add
parseOpCode 2 = Mul
parseOpCode 3 = Read
parseOpCode 4 = Write
parseOpCode 5 = OpJumpTrue
parseOpCode 6 = OpJumpFalse
parseOpCode 7 = OpLT
parseOpCode 8 = OpEQ
parseOpCode _ = H

opCodeSize :: OpCode -> Int
opCodeSize x = case x of
    Add -> 4
    Mul -> 4
    Read -> 2
    Write -> 2
    OpLT -> 4
    OpEQ -> 4
    OpJumpTrue -> 3
    OpJumpFalse -> 3
    _ -> 0

type Pointer = Int
type InputPointer = Int
type OutputPointer = Int

type Parameter = (Mode, InputPointer)

data Instruction
    = Addition Parameter Parameter Parameter
    | Multiply Parameter Parameter Parameter
    | GetInput Parameter
    | WriteOutput Parameter
    | JumpTrue Parameter Parameter
    | JumpFalse Parameter Parameter
    | LessThan Parameter Parameter Parameter
    | Equals Parameter Parameter Parameter
    | Halt

runMachineFullOutput :: [Int] -> [Int] -> [Int]
runMachineFullOutput ls input =
    let (output, state) = runState (runUntilHalt 0 input) (listArray (0, subtract 1 . length $ ls) ls)
    in output

runMachine' :: [Int] -> [Int] -> Int
runMachine' ls input =
    let (output, state) = runState (runUntilHalt 0 input) (listArray (0, subtract 1 . length $ ls) ls)
    in head output

runMachine :: [Int] -> [Int] -> IO ()
runMachine ls input = do
    let res = runState (runUntilHalt 0 input) (listArray (0, subtract 1 . length $ ls) ls)
    print res

runStep :: [Int] -> [Int] -> IO ()
runStep ls input = do
    let res = runState (executeStep 0 input) (listArray (0, subtract 1 . length $ ls) ls)
    print res

runUntilHalt :: Pointer -> [Int] -> IntCode [Int]
runUntilHalt pointer input = go pointer input []
    where go pointer input output = do
            res <- executeStep pointer input
            case res of
                (Ok, newPointer)       -> go newPointer input output
                (DidRead, newPointer)  -> go newPointer (tail input) output
                (Output n, newPointer) -> go newPointer input (n:output)
                (Branch n, _) -> go n input output
                (Stop, _) -> return output


executeStep :: Pointer -> [Int] -> IntCode (StatusCode, Pointer)
executeStep pointer input = do
    (_, upperBound) <- fmap bounds get
    case upperBound < pointer of
        True -> return (Stop, pointer)
        False -> do
            x <- readMemory pointer
            let (modes, code) = parseCodeAndMode x
            instruction <- fetchInstruction pointer modes code
            status <- executeInstruction input instruction
            return (status, pointer + opCodeSize code)

fetchInstruction :: Pointer -> [Mode] -> OpCode -> IntCode Instruction
fetchInstruction pointer modes Add = do
    mem <- readMemoryRange ((+pointer) <$> [1,2,3])
    let params = zip modes mem
    case params of
        [in1, in2, out] -> return $ Addition in1 in2 out
        _ -> error "unreachable"
fetchInstruction pointer modes Mul = do
    mem <- readMemoryRange ((+pointer) <$> [1,2,3])
    let params = zip modes mem
    case params of
        [in1, in2, out] -> return $ Multiply in1 in2 out
        _ -> error "unreachable"
fetchInstruction pointer (mode:_) Read = do
    in1 <- readMemory (pointer + 1)
    return $ GetInput (Immediate, in1)
fetchInstruction pointer (mode:_) Write = do
    in1 <- readMemory (pointer + 1)
    return $ WriteOutput (mode, in1)
fetchInstruction pointer modes OpJumpTrue = do
    mem <- readMemoryRange ((+pointer) <$> [1,2])
    let params = zip modes mem
    case params of
        [in1, branch] -> return $ JumpTrue in1 branch
        _ -> error "unreachable"
fetchInstruction pointer modes OpJumpFalse = do
    mem <- readMemoryRange ((+pointer) <$> [1,2])
    let params = zip modes mem
    case params of
        [in1, branch] -> return $ JumpFalse in1 branch
        _ -> error "unreachable"
fetchInstruction pointer modes OpLT = do
    mem <- readMemoryRange ((+pointer) <$> [1,2,3])
    let params = zip modes mem
    case params of
        [in1, in2, out] -> return $ LessThan in1 in2 out
        _ -> error "unreachable"
fetchInstruction pointer modes OpEQ = do
    mem <- readMemoryRange ((+pointer) <$> [1,2,3])
    let params = zip modes mem
    case params of
        [in1, in2, out] -> return $ Equals in1 in2 out
        _ -> error "unreachable"
    
fetchInstruction _ _ H = return Halt

readParameter :: (Mode, Int) -> IntCode Int
readParameter (Immediate, x) = return x
readParameter (Position, x) = readMemory x

readParameters :: ((Mode, Int), (Mode, Int)) -> IntCode (Int, Int)
readParameters = \(in1, in2) -> do
    a <- readParameter in1
    b <- readParameter in2
    return (a, b)

executeInstruction :: [Int] -> Instruction -> IntCode StatusCode
executeInstruction _ (Addition in1 in2 (_, out)) = do
    (a,b) <- readParameters (in1, in2)
    modify (// [(out, a + b)]) 
    return Ok
executeInstruction _ (Multiply in1 in2 (_, out)) = do
    (a,b) <- readParameters (in1, in2)
    modify (// [(out, a * b)]) 
    return Ok
executeInstruction _ (LessThan in1 in2 (_, out)) = do
    (a,b) <- readParameters (in1, in2)
    case a < b of
        True  -> modify (// [(out, 1)])
        False -> modify (// [(out, 0)])
    return Ok
executeInstruction _ (Equals in1 in2 (_, out)) = do
    (a,b) <- readParameters (in1, in2)
    case a == b of
        True  -> modify (// [(out, 1)])
        False -> modify (// [(out, 0)])
    return Ok
executeInstruction _ (JumpTrue in1 branch) = do
    (a,b) <- readParameters (in1, branch)
    case a of
        0 -> return Ok
        _ -> return (Branch b)
executeInstruction _ (JumpFalse in1 branch) = do
    (a,b) <- readParameters (in1, branch)
    case a of
        0 -> return (Branch b)
        _ -> return Ok
executeInstruction input (GetInput in1) = do
    address <- readParameter in1
    modify (// [(address, head input)])
    return DidRead
executeInstruction input (WriteOutput in1) = do
    a <- readParameter in1
    return $ Output a
executeInstruction _ _ = do
    return Stop

readMemory :: Pointer -> IntCode Int
readMemory pointer = fmap (! pointer) get

readMemoryRange :: [Int] -> IntCode [Int]
readMemoryRange ls = mapM readMemory ls

jumpExample :: [Int]
jumpExample = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]