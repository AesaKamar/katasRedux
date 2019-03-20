{-# LANGUAGE LambdaCase #-}
module Day06_2018 where

import           Text.Parsec.String
import           Text.Parsec.Prim (parse)
import           Text.Parsec.Error (ParseError)
import           Text.ParserCombinators.Parsec.Number ( int )
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Map (Map)
import qualified Data.Map as M
import Data.Foldable
import Common
import Data.Trees.KdTree
import           Data.Maybe (catMaybes)
import Data.List (sortBy, groupBy)
import Control.Monad.Trans.Either


newtype IndexedManhattanPoint =
  IndexedManhattanPoint (Integer, (Integer, Integer))
  deriving (Eq, Ord, Show)

data NearestNeighbor =
  OneNeighbor IndexedManhattanPoint
  | SeveralNeighbors
  deriving (Eq, Ord, Show)

instance Point IndexedManhattanPoint where
  dimension p = 2
  coord 0 (IndexedManhattanPoint (_,a)) = fst a |> fromInteger
  coord 1 (IndexedManhattanPoint (_,a)) = snd a |> fromInteger
  dist2 (IndexedManhattanPoint (_, (ay, ax))) (IndexedManhattanPoint (_,(by, bx))) =
    (abs (ay - by) + abs (ax - bx)) |> fromInteger


minimumBound = 0
maximumBound = 400
coordsToSearch = [minimumBound..maximumBound]
  |> zip [minimumBound..maximumBound]
  |> zip [0..]
  |$> IndexedManhattanPoint

parser :: Parser [IndexedManhattanPoint]
parser = ((((int <* string ", ") `tup` int ) `sepEndBy` newline) <* eof)
  |$> fmap (\x -> IndexedManhattanPoint (0, x))


identifyNearestNeighbor ::  KdTree IndexedManhattanPoint -> IndexedManhattanPoint -> NearestNeighbor
identifyNearestNeighbor kdt p = case kNearestNeighbors kdt 2 p of
  [a, b] -> let
    d = compareDistance a b p
    in case d of
      EQ -> SeveralNeighbors
      LT -> OneNeighbor a
      GT -> OneNeighbor b
  _ -> undefined

-- Take a KD tree of crystal seeds and map all of the coords to nearest neighbors
nearestNeighborMap ::  [IndexedManhattanPoint] -> KdTree IndexedManhattanPoint
                   -> [(IndexedManhattanPoint, NearestNeighbor)]
nearestNeighborMap searchSpace kdt = let
  indexedSearchSpace = searchSpace |$> identifyNearestNeighbor kdt
  in zip searchSpace indexedSearchSpace


divideIntoRegionsByNeighbor :: [(IndexedManhattanPoint, NearestNeighbor)]
                            -> [(Set IndexedManhattanPoint, NearestNeighbor)]
divideIntoRegionsByNeighbor flattenedPts =  let
  groups = groupBy (\a b -> snd a == snd b) flattenedPts
  stuff = groups  |$> (\s -> let
    region = head s |> snd
    pts = s |$> fst
    in (S.fromList pts, region))
  in stuff |> filter (\(_, a) -> case a of
      SeveralNeighbors -> False
      OneNeighbor _ -> True)

repeatedListOfMax = repeat maximumBound |> take (maximumBound |> fromInteger)
repeatedListOfMin = repeat minimumBound |> take (maximumBound |> fromInteger)
wallPoints =
  [(y, x) | x <- [minimumBound..maximumBound], y <- repeatedListOfMin] <>
  [(y, x) | x <- [minimumBound..maximumBound], y <- repeatedListOfMax] <>
  [(y, x) | y <- [minimumBound..maximumBound], x <- repeatedListOfMin] <>
  [(y, x) | y <- [minimumBound..maximumBound], x <- repeatedListOfMax]

-- Does the region contain a wall (boundary condition)
-- THIS IS SUPER SLOW D:
isNonInfiniteRegion ::  (Set IndexedManhattanPoint, NearestNeighbor) -> Bool
isNonInfiniteRegion (s, _) = let
  intersection = S.intersection (wallPoints |$> (\i -> IndexedManhattanPoint (0, i)) |> S.fromList) s
  in intersection |> S.null


-- kd = fromList coordsToSearch
-- solution :: IO String
solution = do
  input <-  readFile "./src/2018/day06/input"
  _ <- print "Hello"
  pts <- pure $ input |> parse parser ""
  -- print pts
  let part1 = do
        indexedPoints <- pts
        -- _ <- error "a"
        let kdt = fromList indexedPoints
        -- _ <- error "b"
        let mapping =  nearestNeighborMap coordsToSearch kdt
        -- _ <- error "c"
        let regions = divideIntoRegionsByNeighbor mapping |> filter isNonInfiniteRegion
        -- _ <- error "d"
        let sortedRegions = regions |> sortBy (\a b -> compare (S.size (fst a)) (S.size (fst b)))
        pure sortedRegions
  pure $ part1
