module Runner
(run)
where

import BF
-- import Parser
import Data.Char (chr, ord)

data Memory
   = Memory
    { readMem :: Int
    , getLeft :: [Int]
    , getRight :: [Int]
    }
  deriving Show

doubleHex :: Int -> String
doubleHex n = [sym d1, sym d2]
  where
    (d1, d2) = n `quotRem` 16
    sym = ((['0' .. '9'] ++ ['A' .. 'F']) !!)

memPP :: Memory -> IO ()
memPP (Memory p ls rs) = putStrLn $ line1 ++ "\n" ++ line2
  where
    line1 = left ++ pointer ++ right
    line2 = leftPadding ++ "^^" ++ rightPadding

    leftPadding = replicate (length left + length pointer - 2) ' '
    rightPadding = replicate (length right) ' '

    pointer = doubleHex p
    left
      | null ls = []
      | otherwise = (unwords $ map doubleHex $ reverse ls) ++ " "
    right
      | null rs = []
      | otherwise = " " ++ (unwords $ map doubleHex $ rs)

newMemory :: Memory
newMemory = Memory 0 [] []

moveLeft :: Memory -> Memory
moveLeft (Memory p [] rs) = Memory 0 [] (p:rs)
moveLeft (Memory p (l:ls) rs) = Memory l ls (p:rs)

moveRight :: Memory -> Memory
moveRight (Memory p ls []) = Memory 0 (p:ls) []
moveRight (Memory p ls (r:rs)) = Memory r (p:ls) rs

modifyMem :: (Int -> Int) -> Memory -> Memory
modifyMem f (Memory p ls rs) = Memory (f p `mod` 256) ls rs

mL, mR, mInc, mDec :: Memory -> Memory
mL = moveLeft
mR = moveRight
mInc = modifyMem (+1)
mDec = modifyMem (subtract 1)

setMem :: Int -> Memory -> Memory
setMem i = modifyMem (const i)

run :: [BF] -> IO Memory
run is = go is newMemory
  where
    go :: [BF] -> Memory -> IO Memory
    go [] mem = return mem
    go (i:is) mem = case i of
      L -> go is (mL mem)
      R -> go is (mR mem)
      Inc -> go is (mInc mem)
      Dec -> go is (mDec mem)
      In -> do
        char <- getChar
        go is (setMem (ord char) mem)
      Out -> do
        _ <- putChar $ chr $ readMem mem
        go is mem
      Loop js -> case readMem mem of
        0 -> go is mem
        otherwise -> do
          mem' <- go js mem
          go (i:is) mem'

-- run :: [BF] -> IO Memory
-- run is = go is newMemory
--   where
--     go :: [BF] -> Memory -> IO Memory
--     go [] mem = do
--       _ <- memPP mem
--       _ <- bfPP []
--       return mem
--
--     go (i:is) mem = case i of
--       L -> do
--         _ <- memPP mem
--         _ <- bfPP (i:is)
--         go is (mL mem)
--
--       R -> do
--         _ <- memPP mem
--         _ <- bfPP (i:is)
--         go is (mR mem)
--
--       Inc -> do
--         _ <- memPP mem
--         _ <- bfPP (i:is)
--         go is (mInc mem)
--
--       Dec -> do
--         _ <- memPP mem
--         _ <- bfPP (i:is)
--         go is (mDec mem)
--
--       In -> do
--         _ <- memPP mem
--         _ <- bfPP (i:is)
--         char <- getChar
--         go is (setMem (ord char) mem)
--
--       Out -> do
--         _ <- memPP mem
--         _ <- bfPP (i:is)
--         _ <- putChar $ chr $ readMem mem
--         go is mem
--
--       Loop js -> case readMem mem of
--         0 -> do
--           _ <- memPP mem
--           _ <- bfPP (i:is)
--           go is mem
--
--         otherwise -> do
--           _ <- memPP mem
--           _ <- bfPP (i:is)
--           mem' <- go js mem
--           go (i:is) mem'

-- Right bf = parse "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+."

-- hw = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
-- hwLong = "\
-- \++++++++               Set Cell #0 to 8\n\
-- \[\n\
-- \    >++++               Add 4 to Cell #1; this will always set Cell #1 to 4\n\
-- \    [                   as the cell will be cleared by the loop\n\
-- \        >++             Add 2 to Cell #2\n\
-- \        >+++            Add 3 to Cell #3\n\
-- \        >+++            Add 3 to Cell #4\n\
-- \        >+              Add 1 to Cell #5\n\
-- \        <<<<-           Decrement the loop counter in Cell #1\n\
-- \    ]                   Loop till Cell #1 is zero; number of iterations is 4\n\
-- \    >+                  Add 1 to Cell #2\n\
-- \    >+                  Add 1 to Cell #3\n\
-- \    >-                  Subtract 1 from Cell #4\n\
-- \    >>+                 Add 1 to Cell #6\n\
-- \    [<]                 Move back to the first zero cell you find; this will\n\
-- \                        be Cell #1 which was cleared by the previous loop\n\
-- \    <-                  Decrement the loop Counter in Cell #0\n\
-- \]                       Loop till Cell #0 is zero; number of iterations is 8\n\
-- \\n\
-- \The result of this is:\n\
-- \Cell No :   0   1   2   3   4   5   6\n\
-- \Contents:   0   0  72 104  88  32   8\n\
-- \Pointer :   ^\n\
-- \\n\
-- \>>.                     Cell #2 has value 72 which is 'H'\n\
-- \>---.                   Subtract 3 from Cell #3 to get 101 which is 'e'\n\
-- \+++++++..+++.           Likewise for 'llo' from Cell #3\n\
-- \>>.                     Cell #5 is 32 for the space\n\
-- \<-.                     Subtract 1 from Cell #4 for 87 to give a 'W'\n\
-- \<.                      Cell #3 was set to 'o' from the end of 'Hello'\n\
-- \+++.------.--------.    Cell #3 for 'rl' and 'd'\n\
-- \>>+.                    Add 1 to Cell #5 gives us an exclamation point\n\
-- \>++.                    And finally a newline from Cell #6"
