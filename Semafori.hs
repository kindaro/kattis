module Main where

import qualified Data.List as List

parseInput :: String -> [[Integer]]
parseInput = ((fmap.fmap) read) . fmap words . lines

main :: IO ()
main = do
    -- Parse input.
    contents <- getContents
    let ([_, x1]: semafori) = parseInput contents
        (t, x) = List.foldl' step (0, 0) semafori
        t1 = t + abs (x1 - x)
    print t1

step :: (Integer, Integer) -> [Integer] -> (Integer, Integer)
step (t, x) [x', tR, tG] =
  let t' = t + abs (x' - x)
      tSemafor = t' `mod` (tR + tG)
      delay = max 0 (tR - tSemafor)
  in  (t' + delay, x')
step _ _ = error $ "Fatal error: wrong semafor descrption."
