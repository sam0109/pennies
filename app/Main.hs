module Main where

-- Takes the first 100 elements of the infinite list of triples
main :: IO ()
main = print (take 100 triples)

-- a_1 = "12" 
-- a_n = a_n-1 + b_n
an :: Int -> [Int]
an 1 = [1, 2]
an n = an (n - 1) ++ bn n

-- b_1 = "2"
-- b_n = a_n-1 + b_n-1
bn :: Int -> [Int]
bn 1 = [2]
bn n = an (n - 1) ++ bn (n - 1)

-- Generates an infinite list of the numbers to skip
skips :: [Int]
skips = [1..] >>= bn

-- Moves from one triple to the next, taking into account the skip
nextTriple :: (Int, Int, Int) -> Int -> (Int, Int, Int)
nextTriple (n, x, _) numToSkip = (n', x', y')
  where n' = n + 1
        x' = x + numToSkip
        y' = n' + x'

-- Generates an infinite list of the triples
triples :: [(Int, Int, Int)]
triples = scanl nextTriple (1, 1, 2) skips