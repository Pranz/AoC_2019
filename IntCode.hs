{-# LANGUAGE BangPatterns #-}
module IntCode where

import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Monad.Identity
import Control.Applicative
import Control.Monad (mapM)
import Control.Monad.Fail

(//) :: M.Map Integer Integer -> [(Integer, Integer)] -> M.Map Integer Integer
m // kvs = foldr (\(k,v) m -> M.insert k v m) m kvs

type IntCode a = State (M.Map Integer Integer) a

data OpCode
    = Add
    | Mul
    | Read
    | Write
    | OpJumpTrue
    | OpJumpFalse
    | OpLT
    | OpEQ
    | OpSetBase
    | H

data Mode = Position | Immediate | Relative

data StatusCode = Ok | Stop | Output Integer | DidRead | Branch Integer
    deriving (Show, Eq)

parseCodeAndMode :: Integer -> ([Mode], OpCode)
parseCodeAndMode x = ( [ parseMode ((x `div` 100  ) `mod` 10)
                       , parseMode ((x `div` 1000 ) `mod` 10)
                       , parseMode ((x `div` 10000) `mod` 10)]
                     , parseOpCode . (`mod` 100) $ x)

parseMode :: Integer -> Mode
parseMode 0 = Position
parseMode 1 = Immediate
parseMode 2 = Relative

parseOpCode :: Integer -> OpCode
parseOpCode 1 = Add
parseOpCode 2 = Mul
parseOpCode 3 = Read
parseOpCode 4 = Write
parseOpCode 5 = OpJumpTrue
parseOpCode 6 = OpJumpFalse
parseOpCode 7 = OpLT
parseOpCode 8 = OpEQ
parseOpCode 9 = OpSetBase
parseOpCode _ = H

opCodeSize :: OpCode -> Integer
opCodeSize x = case x of
    Add -> 4
    Mul -> 4
    Read -> 2
    Write -> 2
    OpLT -> 4
    OpEQ -> 4
    OpJumpTrue -> 3
    OpJumpFalse -> 3
    OpSetBase -> 2
    _ -> 0

type Pointer = Integer
type InputPointer = Integer
type OutputPointer = Integer

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
    | SetRelativeBase Parameter
    | Halt

initialMemory :: [Integer] -> M.Map Integer Integer
initialMemory prog = M.empty // zip [0..] prog

runMachineFullOutput :: [Integer] -> [Integer] -> [Integer]
runMachineFullOutput ls input =
    let (output, state) = runState (runUntilHalt 0 input) (initialMemory ls)
    in output

runMachine' :: [Integer] -> [Integer] -> Integer
runMachine' ls input =
    let (output, state) = runState (runUntilHalt 0 input) (initialMemory ls)
    in head output

runMachine :: [Integer] -> [Integer] -> IO ()
runMachine ls input = do
    let res = runState (runUntilHalt 0 input) (initialMemory ls)
    print res

runStep :: [Integer] -> [Integer] -> IO ()
runStep ls input = do
    let res = runState (executeStep 0 input) (initialMemory ls)
    print res

runUntilHalt :: Pointer -> [Integer] -> IntCode [Integer]
runUntilHalt pointer input = go pointer input []
    where go pointer input output = do
            res <- executeStep pointer input
            case res of
                (Ok, newPointer)       -> go newPointer input output
                (DidRead, newPointer)  -> go newPointer (tail input) output
                (Output n, newPointer) -> go newPointer input (n:output)
                (Branch n, _) -> go n input output
                (Stop, _) -> return output


executeStep :: Pointer -> [Integer] -> IntCode (StatusCode, Pointer)
executeStep pointer input = do
    m <- get
    case M.lookup pointer m of
        Nothing -> return (Stop, pointer)
        Just x -> do
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
    let m = case mode of
                Position -> Immediate
                m' -> m'
    return $ GetInput (m, in1)
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
fetchInstruction pointer (mode:_) OpSetBase = do
    in1 <- readMemory (pointer + 1)
    return $ SetRelativeBase (mode, in1)
fetchInstruction _ _ H = return Halt

readRelativeBase :: IntCode Integer
readRelativeBase = readMemory (negate 1)

setRelativeBase :: Integer -> IntCode ()
setRelativeBase x = modify (// [(negate 1, x)])

readParameter :: (Mode, Integer) -> IntCode Integer
readParameter (Immediate, x) = return x
readParameter (Position, x) = readMemory x
readParameter (Relative, x) = do
    base <- readRelativeBase
    readMemory (base + x)

readWriteParameter :: (Mode, Integer) -> IntCode Integer
readWriteParameter (Immediate, x) = return x
readWriteParameter (Position, x) = return x
readWriteParameter (Relative, x) = fmap (+x) readRelativeBase

readParameters :: ((Mode, Integer), (Mode, Integer)) -> IntCode (Integer, Integer)
readParameters = \(in1, in2) -> do
    a <- readParameter in1
    b <- readParameter in2
    return (a, b)

executeInstruction :: [Integer] -> Instruction -> IntCode StatusCode
executeInstruction _ (Addition in1 in2 out') = do
    (a,b) <- readParameters (in1, in2)
    out <- readWriteParameter out'
    modify (// [(out, a + b)]) 
    return Ok
executeInstruction _ (Multiply in1 in2 out') = do
    (a,b) <- readParameters (in1, in2)
    out <- readWriteParameter out'
    modify (// [(out, a * b)]) 
    return Ok
executeInstruction _ (LessThan in1 in2 out') = do
    (a,b) <- readParameters (in1, in2)
    out <- readWriteParameter out'
    case a < b of
        True  -> modify (// [(out, 1)])
        False -> modify (// [(out, 0)])
    return Ok
executeInstruction _ (Equals in1 in2 out') = do
    (a,b) <- readParameters (in1, in2)
    out <- readWriteParameter out'
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
    address <- readWriteParameter in1
    modify (// [(address, head input)])
    return DidRead
executeInstruction input (WriteOutput in1) = do
    a <- readParameter in1
    return $ Output a
executeInstruction _ (SetRelativeBase in1) = do
    a <- readParameter in1
    prevBase <- readRelativeBase
    setRelativeBase (prevBase + a)
    return Ok
executeInstruction _ _ = do
    return Stop

readMemory :: Pointer -> IntCode Integer
readMemory pointer = fmap (M.findWithDefault 0 pointer) get

readMemoryRange :: [Integer] -> IntCode [Integer]
readMemoryRange ls = mapM readMemory ls

jumpExample :: [Integer]
jumpExample = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
