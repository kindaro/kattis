{-# language RecordWildCards, ScopedTypeVariables #-}

module Main where

import Prelude hiding (map)
import qualified Data.List as List
import Data.Map (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Function
import Data.Maybe

converge :: Eq a => [a] -> [a]
converge = convergeBy (==)

fixp :: Eq a => (a -> a) -> a -> a
fixp f = last . converge . iterate f

convergeBy :: (a -> a -> Bool) -> [a] -> [a]
convergeBy _ [ ] = [ ]
convergeBy _ [x] = [x]
convergeBy eq (x: xs@(y: _))
    | x `eq` y = [x]
    | otherwise = x : convergeBy eq xs

fixpBy :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixpBy eq f = last . convergeBy eq . iterate f

replaceAt :: Int -> Integer -> [Integer] -> [Integer]
replaceAt i y xs =
  let (before, (_: after)) = List.splitAt i xs
  in  before ++ y: after

possiblyDecreaseAt :: Int -> [Integer] -> [Integer]
possiblyDecreaseAt i xs =
  let (before, (x: after)) = List.splitAt i xs
  in  before ++ (if x > 0 then x - 1 else 0): after

newtype Chance a = Chance { chance :: Map a Double } deriving (Show, Eq, Ord)

fromList :: Ord a => [a] -> Chance a
fromList xs = (Chance . Map.fromList . fmap weigh . List.group . List.sort) xs
                                   -- Safe: `group` always returns non-empty.
  where
    weigh :: [a] -> (a, Double)
    weigh ys@(y: _) = (y, (fromIntegral . length) ys / (fromIntegral . length) xs)
    weigh [ ] = error $ "Cannot weigh an empty list."

getChance :: Ord a => Chance a -> a -> Double
getChance Chance{..} x = fromMaybe 0 (chance !? x)

map :: forall a b. (Ord a, Ord b) => (a -> b) -> Chance a -> Chance b
map f = Chance . Map.mapKeysWith (+) f . chance

scale :: forall a. Double -> Chance a -> Chance a
scale x = Chance . fmap (*x) . chance

combine :: Ord a => [Chance a] -> Chance a
combine = let f = Map.unionsWith (+) in Chance . f . fmap chance

bind :: forall a b. (Ord a, Ord b) => (a -> [b]) -> Chance a -> Chance b
bind f = combine . fmap expand . Map.toList . chance
    where expand (x, w) = (scale w . fromList . f) x

data State = Victory | Loss | Continue [Integer] Integer
    deriving (Show, Eq, Ord)

parseInput :: String -> State
parseInput xs = let ys = (fmap read . words) xs in Continue (init ys) (last ys)

main :: IO ()
main = do
    contents <- getContents

    let initial = parseInput contents
        state = (fixpBy enough play . fromList) [initial]

    print (state `getChance` Victory)

play :: Chance State -> Chance State
play = bind step

enough :: Chance State -> Chance State -> Bool
enough u v = metric (getChance u Victory + getChance u Loss) 1 < (1 / 10^(6 :: Int))
    where metric x y = abs (x - y)

step :: State -> [State]
step Loss    = [Loss]
step Victory = [Victory]
step (Continue [ ] 0) = error $ "Fatal error: Trees and raven are both zero."
step (Continue [ ] _) = [Victory]
step (Continue       (_:_) 0    ) = [Loss]
step (Continue trees@(_:_) raven) =
  let (maxTree, maxTreeIndex) = List.maximumBy (compare `on` fst) (trees `zip` [0..])
        -- Safe: `trees` are of non-zero length by pattern match.
      trees' = fmap List.sort
             $ replaceAt maxTreeIndex (maxTree - 1) trees
                            -- Safe: `maxTreeIndex` is within range of `trees` by construction.
             : fmap (\x -> possiblyDecreaseAt x trees) [0 .. length trees - 1]
                            -- Safe: `x` is within range of `trees` by construction.
  in  Continue trees (raven - 1): fmap (flip Continue raven . filter (>0)) trees'
